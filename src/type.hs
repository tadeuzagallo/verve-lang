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
          | TyInterface String String [Type]
          | TyImplementation Type Type
          | TyEnum [Type]
          | TyFunction [Type] Type
          | TyInstance Type [Type]
          deriving (Show, Eq)
