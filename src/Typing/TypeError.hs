module Typing.TypeError
  ( TypeError(..)
  ) where

import Absyn
import Error
import Typing.Types

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

instance Show TypeError where
  show (UnknownVariable x) = "Unknown variable: " ++ x
  show (UnknownType x) = "Unknown type: " ++ x
  show ArityMismatch = "Invalid function call: too many arguments"
  show InferenceFailure = "Failed to infer type arguments for function call"
  show (TypeError expected actual) = "Expected a value of type `" ++ show expected ++ "`, but found `" ++ show actual ++ "`"
  show (UnknownField ty field) = "Trying to access unknown property `" ++ field ++ "` of object of type `" ++ show ty ++ "`"
  show (GenericError err) = err
  show (MissingImplementation name) = "Implementation is missing method `" ++ name ++ "`"
  show (ExtraneousImplementation name) = "Implementation contains method `" ++ name ++ "` which is not part of the interface"
  show (InterfaceExpected ty) = "Expected an interface, but found type `" ++ show ty ++ "`"
  show (MissingInstance intf ty) = "No instance of `" ++ intf ++ "` for type `" ++ show ty ++  "`"

instance ErrorT TypeError where
  kind _ = "TypeError"
