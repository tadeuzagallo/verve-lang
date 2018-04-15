module Typing.TypeError
  ( TypeError(..)
  ) where

import Prelude hiding (print)

import Absyn.Meta
import Typing.Kinds
import Typing.Types
import Util.Error
import Util.PrettyPrint

data TypeError
  = UnknownVariable String
  | UnknownType String
  | UnknownInterface String
  | ArityMismatch
  | TypeArityMismatch
  | InferenceFailure
  | TypeError Type Type
  | UnknownField Type String
  | ImplementationMissingMethod Name
  | ExtraneousImplementation Name
  | InterfaceExpected Type
  | MissingImplementation String Type
  | ImplementationError String Type
  | ImplementingNonInterface String Type
  | KindError Type Kind Kind
  | VariableUsedDuringInitialization Name

instance ErrorT TypeError where
  kind _ = "TypeError"

instance Show TypeError where
  show (UnknownVariable x) =
    "Unknown variable: " ++ pprName x

  show (UnknownType x) =
    "Unknown type: " ++ pprName x

  show (UnknownInterface x) =
    "Unknown interface: " ++ x

  show ArityMismatch =
    "Invalid function call: too many arguments"

  show TypeArityMismatch =
    "Type applied to too many arguments"

  show InferenceFailure =
    "Failed to infer type arguments for function call"

  show (TypeError expected actual) =
    "Expected a value of type `" ++ print expected ++ "`, but found `" ++ print actual ++ "`"

  show (UnknownField ty field) =
    "Trying to access unknown property `" ++ field ++ "` of object of type `" ++ print ty ++ "`"

  show (ImplementationMissingMethod name) =
    "Implementation is missing method `" ++ pprName name ++ "`"

  show (ExtraneousImplementation name) =
    "Implementation contains method `" ++ pprName name ++ "` which is not part of the interface"

  show (InterfaceExpected ty) =
    "Expected an interface, but found type `" ++ print ty ++ "`"

  show (ImplementingNonInterface name actualTy) =
    "Cannot have an implementation of `" ++ pprName name ++ "` because it's not an interface. " ++ pprName name  ++ " has type `" ++ print actualTy ++ "`."

  show (MissingImplementation intf ty) =
    "No implementation of `" ++ pprName intf ++ "` for type `" ++ print ty ++  "`"

  show (ImplementationError intfName ty) =
    "Implementation of `" ++ pprName intfName ++ "` for type `" ++ print ty ++ "` does not use all the type variables it introduces and/or uses concrete types."

  show (KindError ty expectedKind actualKind) =
    "Type `" ++ print ty ++ "` has kind " ++ show actualKind ++ ", but a type of kind " ++ show expectedKind ++ " was expected."

  show (VariableUsedDuringInitialization var) =
    "Variable `" ++ pprName var ++ "` is used during its own initialisation, which will cause an infinite loop."
