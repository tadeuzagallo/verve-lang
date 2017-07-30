module Types
  ( Type(..)
  , int
  , float
  , char
  , string
  , void
  ) where

data Type
  = Con String
  | Arr [Type]
        Type
  deriving (Eq)

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
