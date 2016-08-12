module BytecodeWriter (write_bytecode) where

import AST
import Bytecode
import Section

import Data.Bits (shiftR)
import Data.ByteString (hPut, pack)
import Data.Word (Word8)
import System.IO (Handle, hPutStr, hPutChar)

write_bytecode :: Bytecode -> Handle -> IO ()
write_bytecode bytecode handle =
  write_strings handle (strings bytecode) >>
  write_functions handle (functions bytecode) >>
  write_text handle (text bytecode)

write_section :: Handle -> Section -> IO ()
write_section handle section =
  write_int handle header >>
    write_int handle (toInteger $ fromEnum section)

write_strings :: Handle -> [String] -> IO ()
write_strings handle strings =
  write_section handle Strings  >>
  (sequence $ map (write_null_terminated_string handle) strings) >>
  write_padding handle strings

write_null_terminated_string :: Handle -> String -> IO ()
write_null_terminated_string handle string =
  hPutStr handle string >> hPutChar handle '\0'

write_padding :: Handle -> [String] -> IO ()
write_padding handle strings =
  let lengths = map ((+) 1 . length) strings
      size = sum lengths
      padding_size = (8 - size) `mod` 8
      padding = map (\_ -> 1 :: Word8) [1..padding_size]
   in hPut handle (pack padding)

write_functions :: Handle -> [AST] -> IO ()
write_functions _ _ = return ()

write_text :: Handle -> [Integer] -> IO ()
write_text handle [] = return ()
write_text handle ints =
  write_section handle Text  >>
  write_int handle 1  >> {- Dummy lookup side table size -}
  write_text_data handle ints

write_text_data :: Handle -> [Integer] -> IO ()
write_text_data handle [] = return ()
write_text_data handle ints =
  write_int handle (head ints) >> write_text_data handle (tail ints)

write_int :: Handle -> Integer -> IO ()
write_int handle int =
  hPut handle byte_string
    where byte_string = pack [fromInteger (shiftR int  0) :: Word8,
                              fromInteger (shiftR int  8) :: Word8,
                              fromInteger (shiftR int 16) :: Word8,
                              fromInteger (shiftR int 24) :: Word8,
                              fromInteger (shiftR int 32) :: Word8,
                              fromInteger (shiftR int 40) :: Word8,
                              fromInteger (shiftR int 48) :: Word8,
                              fromInteger (shiftR int 56) :: Word8]
