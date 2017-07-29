import Interpreter
import Parser
import TypeChecker

import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  file:_ <- getArgs
  result <- parseFile file
  case result of
    Left err -> print err
    Right absyn ->
      case infer absyn of
        Left err -> print err
        Right ty ->
          case eval absyn of
            Left err -> print err
            Right value -> printf "%s : %s\n" (show value) (show ty)
