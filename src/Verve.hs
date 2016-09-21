module Verve where

import AST
import BytecodeWriter
import Parser
import Generator
import Naming
import TypeChecker

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (withBinaryFile, IOMode(WriteMode))
import Text.Printf (printf)

main = do
  args <- getArgs
  let file_name = args !! 0
  source <- readFile file_name
  case parseString file_name source of
    Left e -> do
      putStr "Error parsing input:"
      print e
      exitWith (ExitFailure 1)
    Right ast ->
      let nast = naming ast
       in case type_check nast of
            Left (pos, err) -> do
              printf "On file `%s`, line %d, column %d: \n" (file pos) (line pos) (column pos)
              putStr "TypeError: "
              putStrLn err
              exitWith (ExitFailure 1)
            Right _ -> let bytecode = generate nast
                        in withBinaryFile (args !! 1) WriteMode (write_bytecode bytecode)
