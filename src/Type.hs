module Type where

import qualified Data.Map as Map

data TyType = TyChar
            | TyInt
            | TyFloat
            | TyVoid
            | TyBool
            | TyString
            | TyGeneric String
            | TyEmptyGeneric String
            | TyInterface { ty_name :: String, ty_variable :: String, ty_functions :: [TyType], ty_implementations :: [TyType] }
            | TyImplementation TyType TyType
            | TyEnum [TyType]
            | TyDataInst TyType [TyType]
            | TyFnType [TyType] TyType
            | TyCtor [TyType] String
            | TyFunction String TyType
            | TyAbstractFunction TyType String
            | TyAbsInst TyType TyType
            deriving (Show, Eq)
