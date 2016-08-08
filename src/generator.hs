module Generator where

import AST
import Opcode
import Section

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (hPut, pack)
import Data.Word (Word8)
import System.IO (Handle, hPrint)

data Bytecode = Bytecode {
  text :: [Integer],
  strings :: [String],
  functions :: [AST]
} deriving (Show)

write_int :: Handle -> Integer -> IO ()
write_int handle int =
  hPut handle byte_string
    where byte_string = pack ([fromInteger (shiftR int  0) :: Word8,
                               fromInteger (shiftR int  8) :: Word8,
                               fromInteger (shiftR int 16) :: Word8,
                               fromInteger (shiftR int 24) :: Word8,
                               fromInteger (shiftR int 32) :: Word8,
                               fromInteger (shiftR int 40) :: Word8,
                               fromInteger (shiftR int 48) :: Word8,
                               fromInteger (shiftR int 56) :: Word8 ])

write_section :: Handle -> Section -> IO ()
write_section handle section =
  write_int handle header >>
    write_int handle (toInteger $ fromEnum section)

write_data :: Handle -> [Integer] -> IO ()
write_data handle [] = return ()
write_data handle ints =
  write_int handle (head ints) >> write_data handle (tail ints)

write_bytecode :: Handle -> Bytecode -> IO ()
write_bytecode handle bytecode =
  write_section handle Text  >>
    write_int handle 1  >> {- Dummy lookup side table size -}
      write_data handle (text bytecode)

generate :: AST -> Handle -> IO ()
generate program handle =
  let bytecode = generate_node (Bytecode [] [] []) program
   in write_bytecode handle bytecode

emit_opcode :: Opcode -> Bytecode -> Bytecode
emit_opcode op bytecode =
  Bytecode ((text bytecode) ++ [toInteger $ fromEnum op]) (strings bytecode) (functions bytecode)

write :: Integer -> Bytecode -> Bytecode
write value bytecode =
  Bytecode ((text bytecode) ++ [value]) (strings bytecode) (functions bytecode)

unique_string :: String -> Bytecode -> (Bytecode, Integer)
unique_string str bytecode =
  (bytecode, toInteger 0)

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
       in write string_id bc1

generate_node bytecode (List items) =
  let bc = emit_opcode Op_alloc_list bytecode
   in let bc1 = write (toInteger ((length items) + 1)) bc
       in foldl generate_item bc1 items
      where generate_item bytecode item = let bc = generate_node bytecode item
                                           in let bc1 = emit_opcode Op_obj_store_at bc
                                               in write 1 bc1
