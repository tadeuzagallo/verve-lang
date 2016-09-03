module Verve where

import BytecodeWriter
import Parser
import Generator
import Naming
import TypeChecker

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (withBinaryFile, IOMode(WriteMode))

main = do
  args <- getArgs
  c <- readFile (args !! 0)
  case parseString c of
    Left e -> do
      putStr "Error parsing input:"
      print e
      exitWith (ExitFailure 1)
    Right ast ->
      let nast = naming ast
       in case type_check nast of
            Left e -> putStr "TypeError: " >> putStrLn e >> exitWith (ExitFailure 1)
            Right _ -> let bytecode = generate nast
                        in withBinaryFile (args !! 1) WriteMode (write_bytecode bytecode)
