import Parser

import System.Environment (getArgs)

main :: IO ()
main = do
  file:_ <- getArgs
  result <- parseFile file
  case result of
    Left err -> print err
    Right absyn -> print absyn
