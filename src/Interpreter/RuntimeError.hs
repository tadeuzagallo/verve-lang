module Interpreter.RuntimeError
  ( RuntimeError(..)
  ) where

import Util.Error (ErrorT(kind))

data RuntimeError
  = Unsupported
  | UnknownVariable String
  | MatchFailure
  deriving (Show)

instance ErrorT RuntimeError where
  kind _ = "RuntimeError"
