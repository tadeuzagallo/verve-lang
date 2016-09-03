module Type where

data Type = TyChar
          | TyInt
          | TyFloat
          | TyVoid
          | TyBool
          | TyString
          | TyGeneric String
          | TyInterface String
          | TyImplementation Type Type
          | TyEnum [Type]
          | TyFunction [Type] Type
          | TyInstance Type [Type]
          deriving (Show)
