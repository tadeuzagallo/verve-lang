module Interpreter.RuntimeError
  ( RuntimeError(..)
  , mkError
  ) where

import Util.Error (ErrorT(kind), mkError)

data RuntimeError
  = Unsupported
  | UnknownVariable String
  | MatchFailure
  deriving (Show)

instance ErrorT RuntimeError where
  kind _ = "RuntimeError"
