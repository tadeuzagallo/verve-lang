module ErrorReporter (reportError) where

import AST
import Text.Printf

reportError :: String -> SourcePos -> String -> IO ()
reportError kind pos msg = do
  source <- readFile (file pos)
  printf "%s: %s at %d:%d\n" kind msg (line pos) (column pos)
  printf "On file `%s` at %d:%d\n" (file pos) (line pos) (column pos)
  printf "%d: %s\n" (line pos) (lines source !! (line pos - 1))
  printf ("%" ++ (show $ column pos + 2 + (length . show $ line pos)) ++ "s\n") "^"
