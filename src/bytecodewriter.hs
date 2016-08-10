module BytecodeWriter (write_bytecode) where

import Bytecode
import Section

import Data.Bits (shiftR)
import Data.ByteString (hPut, pack)
import Data.Word (Word8)
import System.IO (Handle, hPutStr, hPutChar)

write_bytecode :: Bytecode -> Handle -> IO ()
write_bytecode bytecode handle =
  write_section handle Strings  >>
    write_strings handle (strings bytecode) >>
      hPut handle (pack (map (\f -> 1 :: Word8) [1..((8 - (sum (map (((+) 1) . length) (strings bytecode))) `mod` 8))])) >>
        write_section handle Text  >>
          write_int handle 1  >> {- Dummy lookup side table size -}
            write_data handle (text bytecode)

write_section :: Handle -> Section -> IO ()
write_section handle section =
  write_int handle header >>
    write_int handle (toInteger $ fromEnum section)

write_strings :: Handle -> [String] -> IO [()]
write_strings handle strings =
  sequence $ map (\s -> hPutStr handle s >> hPutChar handle '\0') strings

write_data :: Handle -> [Integer] -> IO ()
write_data handle [] = return ()
write_data handle ints =
  write_int handle (head ints) >> write_data handle (tail ints)

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
