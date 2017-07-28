import Interpreter
import Parser

import System.Environment (getArgs)

main :: IO ()
main = do
  file:_ <- getArgs
  result <- parseFile file
  case result of
    Left err -> print err
    Right absyn ->
      case eval absyn of
        Left err -> print err
        Right value -> print value
