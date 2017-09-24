module PrettyPrint
  ( PrettyPrint(..)
  , pprName
  ) where

class PrettyPrint a where
  ppr :: a -> String

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
  ppr (a, b) = ppr a ++ " : " ++ ppr b

pprName :: String -> String
pprName = reverse . takeWhile (/= '.') . reverse
