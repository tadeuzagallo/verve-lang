module Generator (generate) where

import AST hiding (functions)
import Bytecode
import Opcode

import Control.Monad.State (State, state, get, put, evalState)
import Data.Bits (shiftL, (.|.))
import Data.List (elemIndex)

initialState = Bytecode
  {
    text = [],
    strings = [],
    functions = []
  }

generate :: AST -> Bytecode
generate program =
  evalState (generate_node program >> get) initialState

emit_opcode :: Opcode -> State Bytecode ()
emit_opcode op = do
  bc <- get
  put bc { text = (text bc) ++ [toInteger $ fromEnum op] }

write :: Integer -> State Bytecode ()
write value = do
  bc <- get
  put bc { text = (text bc) ++ [value] }

unique_string :: String -> State Bytecode ()
unique_string str = do
  bc <- get
  case elemIndex str (strings bc) of
    Just index -> write $ toInteger index
    Nothing -> let id = toInteger $ length (strings bc)
                in do { put bc { strings = (strings bc) ++ [str] }; write id }


decode_double :: Double -> Integer
decode_double double =
  let (significand, exponent) = decodeFloat double
   in (shiftL (toInteger exponent) 53) .|. significand

generate_node :: AST -> State Bytecode ()
generate_node (Program imports body) = do
  mapM_ generate_node imports
  mapM_ generate_node body
  emit_opcode Op_exit

generate_node (Import _ _ _) = return ()

generate_node (Block nodes) =
  mapM_ generate_node nodes

generate_node (Number a) = do
  emit_opcode Op_push
  (case a of
     Left a -> write (toInteger a)
     Right a -> write (decode_double a))

generate_node (String str) = do
  emit_opcode Op_load_string
  unique_string str

generate_node (Identifier name) = do
  emit_opcode Op_lookup
  unique_string name
  write 0 -- lookup cache id - empty for now

generate_node (List items) = do
  emit_opcode Op_alloc_list
  write (toInteger ((length items) + 1))
  mapM_ generate_item items
    where generate_item item = generate_node item >> emit_opcode Op_obj_store_at >> write 1

generate_node (FunctionParameter _ index _) = do
  emit_opcode Op_push_arg
  write (toInteger index)

generate_node (Call callee args) = do
  mapM_ generate_node (reverse args)
  generate_node callee
  emit_opcode Op_call
  write (toInteger $ length args)

generate_node (Function name generics params ret_type body) = do
  bc <- get
  emit_opcode Op_create_closure
  write (toInteger . length $ functions bc)
  write 0 -- capturesScope
  (case name of
     "_" -> return ()
     _   -> emit_opcode Op_bind >> unique_string name)
  generate_function_source (Function name generics params ret_type body)

generate_node (Extern _) = return ()

generate_function_source :: AST -> State Bytecode ()
generate_function_source (Function name generics params ret_type body) = do
  bc <- get
  put initialState { strings = strings bc }
  unique_string name
  write (toInteger $ length params)
  mapM_ param_name params
  generate_node body
  emit_opcode Op_ret
  bc2 <- get
  put $ bc {
    strings = strings bc2,
    functions = ((functions bc) ++ (functions bc2) ++ [text bc2])
           }

param_name :: AST -> State Bytecode ()
param_name (FunctionParameter name _ _) =
  unique_string name
