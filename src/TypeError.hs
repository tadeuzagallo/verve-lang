module TypeError
  ( TypeError(..)
  ) where

import Absyn
import Error
import Types

data TypeError
  = UnknownVariable String
  | UnknownType String
  | ArityMismatch
  | InferenceFailure
  | TypeError Type Type
  | UnknownField Type String
  | GenericError String
  | MissingImplementation Name
  | ExtraneousImplementation Name
  | InterfaceExpected Type
  | MissingInstance String Type
  deriving (Show)

instance ErrorT TypeError where
  kind _ = "TypeError"
