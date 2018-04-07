module Renamer.Error
  ( RenamerError(..)
  , throwError
  ) where

import Util.Error

data RenamerError
  = UnknownVariable String

instance Show RenamerError where
  show (UnknownVariable x) =
    "Unknown variable: " ++ x

instance ErrorT RenamerError where
  kind _ = "RenamerError"
