module Bytecode where

data Bytecode = Bytecode {
  text :: [Integer],
  strings :: [String],
  functions :: [[Integer]]
} deriving (Show)
