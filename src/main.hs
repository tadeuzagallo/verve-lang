module Verve where

import BytecodeWriter
import Parser
import Generator
import Naming

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
      let nast = naming ast
          bytecode = generate nast
       in withBinaryFile (args !! 0) WriteMode (write_bytecode bytecode)
