module Renamer.Error
  ( RenamerError(..)
  , throwError
  ) where

import Renamer.State
import Util.Error

import qualified Control.Monad.Except as Except (throwError)

data RenamerError
  = UnknownVariable String

instance Show RenamerError where
  show (UnknownVariable x) =
    "Unknown variable: " ++ x

instance ErrorT RenamerError where
  kind _ = "RenamerError"

throwError :: ErrorT a => a -> Rn b
throwError = Except.throwError . Error

