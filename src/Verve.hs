module Verve where

import BytecodeWriter
import Parser
import Generator
import Naming
import TypeChecker

import System.Environment (getArgs)
import System.IO (withBinaryFile, IOMode(WriteMode))

main = do
  args <- getArgs
  c <- readFile (args !! 0)
  case parseString c of
    Left e -> do
      putStr "Error parsing input:"
      print e
    Right ast ->
      let nast = naming ast
       in case type_check nast of
            Left e -> putStr "TypeError: " >> putStrLn e
            Right _ -> let bytecode = generate nast
                        in withBinaryFile (args !! 1) WriteMode (write_bytecode bytecode)
