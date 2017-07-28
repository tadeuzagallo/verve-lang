module Interpreter
  ( eval
  ) where

import Absyn

data RuntimeError =
  Unsupported
  deriving (Show)

eval :: Expr -> Either RuntimeError Literal
eval (Literal s) = return s
eval _ = fail "Unsupported"
