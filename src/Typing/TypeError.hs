module Typing.TypeError
  ( TypeError(..)
  ) where

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
  | GenericError String
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
  show (GenericError err) =
    err

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
    "Expected a value of type `" ++ ppr expected ++ "`, but found `" ++ ppr actual ++ "`"

  show (UnknownField ty field) =
    "Trying to access unknown property `" ++ field ++ "` of object of type `" ++ ppr ty ++ "`"

  show (ImplementationMissingMethod name) =
    "Implementation is missing method `" ++ pprName name ++ "`"

  show (ExtraneousImplementation name) =
    "Implementation contains method `" ++ pprName name ++ "` which is not part of the interface"

  show (InterfaceExpected ty) =
    "Expected an interface, but found type `" ++ ppr ty ++ "`"

  show (ImplementingNonInterface name actualTy) =
    "Cannot have an implementation of `" ++ pprName name ++ "` because it's not an interface. " ++ pprName name  ++ " has type `" ++ ppr actualTy ++ "`."

  show (MissingImplementation intf ty) =
    "No implementation of `" ++ pprName intf ++ "` for type `" ++ ppr ty ++  "`"

  show (ImplementationError intfName ty) =
    "Implementation of `" ++ pprName intfName ++ "` for type `" ++ ppr ty ++ "` does not use all the type variables it introduces and/or uses concrete types."

  show (KindError ty expectedKind actualKind) =
    "Type `" ++ ppr ty ++ "` has kind " ++ show actualKind ++ ", but a type of kind " ++ show expectedKind ++ " was expected."

  show (VariableUsedDuringInitialization var) =
    "Variable `" ++ var ++ "` is used during its own initialisation, which will cause an infinite loop."
