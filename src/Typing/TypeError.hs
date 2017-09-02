module Typing.TypeError
  ( TypeError(..)
  ) where

import Absyn.Meta
import Error
import Typing.Types

data TypeError
  = UnknownVariable String
  | UnknownType String
  | ArityMismatch
  | TypeArityMismatch
  | InferenceFailure
  | TypeError Type Type
  | UnknownField Type String
  | GenericError String
  | MissingImplementation Name
  | ExtraneousImplementation Name
  | InterfaceExpected Type
  | MissingInstance String Type
  | ImplementationError String Type

instance ErrorT TypeError where
  kind _ = "TypeError"

instance Show TypeError where
  show (GenericError err) =
    err

  show (UnknownVariable x) =
    "Unknown variable: " ++ x

  show (UnknownType x) =
    "Unknown type: " ++ x

  show ArityMismatch =
    "Invalid function call: too many arguments"

  show TypeArityMismatch =
    "Type applied to too many arguments"

  show InferenceFailure =
    "Failed to infer type arguments for function call"

  show (TypeError expected actual) =
    "Expected a value of type `" ++ show expected ++ "`, but found `" ++ show actual ++ "`"

  show (UnknownField ty field) =
    "Trying to access unknown property `" ++ field ++ "` of object of type `" ++ show ty ++ "`"

  show (MissingImplementation name) =
    "Implementation is missing method `" ++ name ++ "`"

  show (ExtraneousImplementation name) =
    "Implementation contains method `" ++ name ++ "` which is not part of the interface"

  show (InterfaceExpected ty) =
    "Expected an interface, but found type `" ++ show ty ++ "`"

  show (MissingInstance intf ty) =
    "No instance of `" ++ intf ++ "` for type `" ++ show ty ++  "`"

  show (ImplementationError intfName ty) =
    "Implementation of `" ++ intfName ++ "` for type `" ++ show ty ++ "` does not use all the type variables it introduces and/or uses concrete types."
