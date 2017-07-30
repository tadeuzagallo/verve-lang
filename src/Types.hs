module Types
  ( Type(..)
  , int
  , float
  , char
  , string
  , void
  ) where

import Data.List (intercalate)
import Text.Printf (printf)

data Type
  = Con String
  | Arr [Type]
        Type
  deriving (Eq)

instance Show Type where
  show (Con t) = t
  show (Arr t1 t2) =
    printf "(%s) -> %s" (intercalate ", " $ map show t1) (show t2)

int :: Type
int = Con "Int"

float :: Type
float = Con "Float"

char :: Type
char = Con "Char"

string :: Type
string = Con "String"

void :: Type
void = Con "Void"
