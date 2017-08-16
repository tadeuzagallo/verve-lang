module Types
  ( Type(..)
  , (<:)
  , fv
  , subst
  , int
  , float
  , char
  , string
  , void
  ) where

import Data.List ((\\), intercalate, union)
import Text.Printf (printf)

data Type
  = Con String
  | Var String
  | Fun [String] [Type] Type
  | Rec [(String, Type)]
  | Type
  | Top
  | Bot
  deriving (Eq)

-- Subtyping
(<:) :: Type -> Type -> Bool

-- S-Refl
t <: u | t == u = True

-- S-Top
_ <: Top = True

-- S-Bot
Bot <: _ = True

-- S-Fun
(Fun v1 p1 t1) <: (Fun v2 p2 t2) =
  v1 == v2 && all (uncurry (<:)) (zip p2 p1) && t1 <: t2

_ <: _ = False

-- Free Type Variables
fv :: Type -> [String]
fv (Var x) = [x]
fv (Fun x s r) =
 (foldl union [] (map fv s) `union` fv r) \\ x
fv _ = []

instance Show Type where
  show (Con t) = t
  show (Var t) = t
  show Type = "Type"
  show Top = "⊤"
  show Bot = "⊥"

  show (Fun gs [v] t2) | v == void = show (Fun gs [] t2)
  show (Fun gs t1 t2) =
    printf "%s(%s) -> %s"
      (if null gs then "" else printf "∀%s. " (intercalate " " gs))
      (intercalate ", " $ map show t1)
      (show t2)

  show (Rec fields) =
    "{" ++ fields' ++ "}"
      where
        fields' = intercalate ", " $ map showField fields
        showField (key, value) = key ++ ": " ++ show value

subst :: [(String, Type)] -> Type -> Type
subst s (Var v) =
  case lookup v s of
    Nothing -> Var v
    Just t -> t
subst s (Fun gs t1 t2) =
  let s' = filter (not . flip elem gs . fst) s
   in Fun gs (map (subst s') t1) (subst s' t2)
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
