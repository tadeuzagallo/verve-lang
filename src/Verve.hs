module Verve where

import BytecodeWriter
import DeSugar
import ErrorReporter
import Generator
import Naming
import Parser
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
          (tast, ctx) = type_check nast
       in case tast of
            Left (pos, err) -> do
              reportError "TypeError" pos err
              exitWith (ExitFailure 1)
            Right tast -> let bytecode = generate ctx $ desugar ctx tast
                           in withBinaryFile (args !! 1) WriteMode (write_bytecode bytecode)
