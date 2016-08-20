module Generator (generate) where

import AST
import Bytecode
import Opcode

import Data.Bits (shiftL, (.|.))
import Data.List (elemIndex)

generate :: AST -> Bytecode
generate program =
  generate_node (Bytecode [] [] []) program

emit_opcode :: Opcode -> Bytecode -> Bytecode
emit_opcode op bytecode =
  Bytecode ((text bytecode) ++ [toInteger $ fromEnum op]) (strings bytecode) (functions bytecode)

write :: Integer -> Bytecode -> Bytecode
write value bytecode =
  Bytecode ((text bytecode) ++ [value]) (strings bytecode) (functions bytecode)

unique_string :: String -> Bytecode -> (Bytecode, Integer)
unique_string str bytecode =
  case elemIndex str (strings bytecode) of
    Just index -> (bytecode, toInteger index)
    Nothing -> let id = toInteger $ length (strings bytecode)
                in let bc = Bytecode (text bytecode) ((strings bytecode) ++ [str]) (functions bytecode)
                    in (bc, id)

decode_double :: Double -> Integer
decode_double double =
  let (significand, exponent) = decodeFloat double
   in (shiftL (toInteger exponent) 53) .|. significand

generate_node :: Bytecode -> AST -> Bytecode

generate_node bytecode (Program imports body) =
  let bc = foldl (\bytecode -> \node -> generate_node bytecode node) bytecode imports
   in let bc1 = foldl generate_node bc body
       in emit_opcode Op_exit bc1

generate_node bytecode (Import pattern path alias) =
  bytecode

generate_node bytecode (Block nodes) =
  foldl generate_node bytecode nodes

generate_node bytecode (Number a) =
  let bc = emit_opcode Op_push bytecode
   in (case a of
         Left a -> write (toInteger a) bc
         Right a -> write (decode_double a) bc)

generate_node bytecode (String str) =
  let bc = emit_opcode Op_load_string bytecode
   in let (bc1, string_id) = unique_string str bc
       in write string_id bc1

generate_node bytecode (Identifier name) =
  let bc = emit_opcode Op_lookup bytecode
   in let (bc1, string_id) = unique_string name bc
       in let bc2 = write string_id bc1
           in write 0 bc2

generate_node bytecode (List items) =
  let bc = emit_opcode Op_alloc_list bytecode
   in let bc1 = write (toInteger ((length items) + 1)) bc
       in foldl generate_item bc1 items
      where generate_item bytecode item = let bc = generate_node bytecode item
                                           in let bc1 = emit_opcode Op_obj_store_at bc
                                               in write 1 bc1

generate_node bytecode (FunctionParameter _ index _) =
  let bc = emit_opcode Op_push_arg bytecode
   in write (toInteger index) bc

generate_node bytecode (Call callee args) =
  let bc = foldl generate_node bytecode (reverse args)
   in let bc1 = generate_node bc callee
       in let bc2 = emit_opcode Op_call bc1
           in write (toInteger $ length args) bc2

generate_node bytecode (Function name params ret_type body) =
  let bc = emit_opcode Op_create_closure bytecode
   in let bc1 = write (toInteger . length $ functions bytecode) bc
       in let bc2 = write 0 bc1 {- capturesScope -}
           in let bc3 = (case name of
                          "_" -> bc2
                          _ ->
                            let bc3 = emit_opcode Op_bind bc2
                             in let (bc4, string_id) = unique_string name bc3
                                 in write string_id bc4)
               in generate_function_source bc3 (Function name params ret_type body)

generate_function_source :: Bytecode -> AST -> Bytecode
generate_function_source bytecode (Function name params ret_type body) =
  let bc = (Bytecode [] (strings bytecode) [])
   in let (bc1, string_id) = unique_string name bc
       in let bc2 = write string_id bc1
           in let bc3 = write (toInteger $ length params) bc2
               in let bc4 = foldl param_name bc3 params
                   in let bc5 = generate_node bc4 body
                       in let bc6 = emit_opcode Op_ret bc5
                           in Bytecode (text bytecode) (strings bc6) ((functions bytecode) ++ (functions bc6) ++ [text bc6])

param_name bc (FunctionParameter name _ _) =
  let (bc1, string_id) = unique_string name bc
   in write string_id bc1
