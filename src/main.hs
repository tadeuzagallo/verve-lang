module Verve where

import BytecodeWriter
import Parser
import Generator

import System.Environment (getArgs)
import System.IO (withBinaryFile, IOMode(WriteMode))

main = do
  args <- getArgs
  c <- getContents
  case parseString c of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right ast ->
      let bytecode = generate ast
       in withBinaryFile (args !! 0) WriteMode (write_bytecode bytecode)
