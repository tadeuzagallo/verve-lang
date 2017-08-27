module Types
  ( Type(..)
  , Var()
  , unsafeFreshVar
  , (<:)
  , fv
  , subst
  , bool
  , int
  , float
  , char
  , string
  , void
  , list
  , genericList
  , var
  , (~>)
  ) where

import Data.List ((\\), intercalate, union)
import Text.Printf (printf)

data Var = TV String Int
  deriving (Eq)

instance Ord Var where
  compare (TV x m) (TV y n) =
    case compare x y of
      EQ -> compare m n
      ord -> ord

instance Show Var where
  show (TV v _) = v

unsafeFreshVar :: Var -> Int -> Var
unsafeFreshVar (TV name _) = TV name

data Type
  = Con String
  | Var Var
  | Fun [Var] [Type] Type
  | Rec [(String, Type)]
  | Cls String [(String, Type)]
  | TyAbs [Var] Type
  | TyApp Type [Type]
  | Top
  | Bot
  | Type
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

_t1@(TyAbs gen t12) <: t2@(TyApp _t21 args) =
  let t1' = subst (zip gen args) t12
   in t1' <: t2

(TyApp t11 t12) <: (TyApp t21 t22) =
  t11 <: t21 && and (zipWith (<:) t12 t22)

_ <: _ = False

-- Free Type Variables
fv :: Type -> [Var]
-- No free variables
fv Top = []
fv Bot = []
fv Type = []
fv (Con _) = []

-- Var
fv (Var x) = [x]

-- Combinations
fv (Fun x s r) =
 (foldl union [] (map fv s) `union` fv r) \\ x
fv (Rec fields) =
  foldl union [] $ map (fv . snd) fields
fv (Cls _ fields) =
  foldl union [] $ map (fv . snd) fields
fv (TyAbs gen ty) =
  fv ty \\ gen
fv (TyApp ty args) =
 (foldl union [] (map fv args) `union` fv ty)

instance Show Type where
  show (Con t) = t
  show (Var v) = show v
  show (Cls t _) = t
  show Type = "Type"
  show Top = "⊤"
  show Bot = "⊥"

  show (Fun gs [v] t2) | v == void = show (Fun gs [] t2)
  show (Fun gs t1 t2) =
    printf "%s(%s) -> %s"
      (if null gs then "" else printf "∀%s. " (intercalate " " $ map show gs))
      (intercalate ", " $ map show t1)
      (show t2)

  show (Rec fields) =
    "{" ++ fields' ++ "}"
      where
        fields' = intercalate ", " $ map showField fields
        showField (key, value) = key ++ ": " ++ show value

  show (TyAbs params ty) =
    "∀" ++ (intercalate " " $ map show params)  ++ "." ++ show ty

  show (TyApp ty args) =
    show ty ++ "<" ++ intercalate ", " (map show args) ++ ">"

subst :: [(Var, Type)] -> Type -> Type
subst _ (Con c) = Con c
subst _ Top = Top
subst _ Bot = Bot
subst _ Type = Type
subst s (Var v) =
  case lookup v s of
    Nothing -> Var v
    Just t -> t
subst s (Fun gs t1 t2) =
  let s' = filter (not . flip elem gs . fst) s
   in Fun gs (map (subst s') t1) (subst s' t2)
subst s (Rec fields) =
  Rec (map (\(k, v) -> (k, subst s v)) fields)
subst s (Cls name vars) =
  Cls name (map (\(k, v) -> (k, subst s v)) vars)
subst s (TyAbs gen ty) =
  let s' = filter (not . flip elem gen . fst) s
   in TyAbs gen (subst s' ty)
subst s (TyApp t1 t2) =
  TyApp (subst s t1) (map (subst s) t2)

bool :: Type
bool = Con "Bool"

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

list :: Type -> Type
list ty = TyApp (Con "List") [ty]

genericList :: Type
genericList = [var "T"] `TyAbs` (list . Var $ var "T")

var :: String -> Var
var v = TV v 0

(~>) :: [Type] -> Type -> Type
(~>) = Fun []
