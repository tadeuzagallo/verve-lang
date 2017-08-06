module Types
  ( Type(..)
  , subst
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
  | Var String
  | Arr [Type] Type
  | Forall [String] Type
  | Type
  deriving (Eq)

instance Show Type where
  show (Con t) = t
  show (Var t) = t
  show Type = "Type"

  show (Forall [] ty) =
    show ty
  show (Forall vars ty) =
    printf "∀ %s. %s" (intercalate " " vars) (show ty)

  show (Arr [v] t2)
    | v == void = show (Arr [] t2)
  show (Arr t1 t2) =
    printf "(%s) -> %s" (intercalate ", " $ map show t1) (show t2)

subst :: (String, Type) -> Type -> Type
subst (v, t) (Var v') | v == v' = t
subst s (Arr t1 t2) = Arr (map (subst s) t1) (subst s t2)
subst (v, t) (Forall vars ty) =
  if elem v vars
     then Forall vars t
     else Forall vars $ subst (v, t) ty
subst _ t = t

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
