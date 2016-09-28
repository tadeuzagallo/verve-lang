module Bytecode where

import qualified Data.Map as Map

data Bytecode = Bytecode {
  text :: [Integer],
  strings :: [String],
  functions :: [[Integer]],
  type_maps :: Map.Map Integer [Integer]
} deriving (Show)
