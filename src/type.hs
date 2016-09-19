module Type where

import qualified Data.Map as Map

data Type = TyChar
          | TyInt
          | TyFloat
          | TyVoid
          | TyBool
          | TyString
          | TyGeneric String
          | TyEmptyGeneric String
          | TyInterface { ty_name :: String, ty_variable :: String, ty_functions :: [Type], ty_implementations :: [Type] }
          | TyImplementation Type Type
          | TyEnum [Type]
          | TyFunction [Type] Type
          | TyAbstractFunction Type String
          | TyInstance Type [Type]
          deriving (Show, Eq)
