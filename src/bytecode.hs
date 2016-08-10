module Bytecode where

import AST

data Bytecode = Bytecode {
  text :: [Integer],
  strings :: [String],
  functions :: [AST]
} deriving (Show)
