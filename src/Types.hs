module Types
  ( Type(..)
  , Var()
  , unsafeFreshVar
  , unsafeNewVar
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
  , hole
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

unsafeNewVar :: String -> Int -> Var
unsafeNewVar = TV

data Type
  = Con String
  | Var Var [Type]
  | Fun [(Var, [Type])] [Type] Type
  | Rec [(String, Type)]
  | Cls String [(String, Type)]
  | TyAbs [Var] Type
  | TyApp Type [Type]
  | Intf String Var [(String, Type)]
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

(Rec r1) <: (Rec r2) =
  all aux r2
    where
      aux (k, t2) = case lookup k r1 of
        Nothing -> False
        Just t1 -> t1 <: t2

_ <: _ = False

-- Free Type Variables
fv :: Type -> [Var]
-- No free variables
fv Top = []
fv Bot = []
fv Type = []
fv (Con _) = []

-- Var
fv (Var x _) = [x]

-- Combinations
fv (Fun x s r) =
  (foldl union [] (map fv s) `union` fv r) \\ map fst x
fv (Rec fields) =
  foldl union [] $ map (fv . snd) fields
fv (Cls _ fields) =
  foldl union [] $ map (fv . snd) fields
fv (TyAbs gen ty) =
  fv ty \\ gen
fv (TyApp ty args) =
 foldl union [] (map fv args) `union` fv ty
fv (Intf _ param methods) =
  (foldl union [] $ map (fv . snd) methods) \\ [param]

instance Show Type where
  show (Con t) = t
  show (Var v _) = show v
  show (Cls t _) = t
  show (Intf t _ _) = t
  show Type = "Type"
  show Top = "⊤"
  show Bot = "⊥"

  show (Fun gs [v] t2) | v == void = show (Fun gs [] t2)
  show (Fun gs t1 t2) =
    printf "%s(%s) -> %s"
      (if null gs then "" else printf "∀%s. " (intercalate " " $ map var gs))
      (intercalate ", " $ map show t1)
      (show t2)
        where
          var (v, []) = show v
          var (v, [t]) = printf "(%s: %s)" (show v) (show t)
          var (v, ts) = printf "(%s: (%s))" (show v) (intercalate ", " $ map show ts)

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
subst s var@(Var v _) =
  case lookup v s of
    Nothing -> var
    Just t -> t
subst s (Fun gs t1 t2) =
  let gs' = map fst gs
      s' = filter (not . flip elem gs' . fst) s
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
subst s (Intf name param methods) =
  let s' = filter ((/=) param . fst) s
   in Intf name param (map (fmap $ subst s') methods)

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
genericList = [var "T"] `TyAbs` (list $ Var (var "T") [])

var :: String -> Var
var v = TV v (-1)

hole :: Type
hole = Var (TV "#hole" (-42)) [Bot]

(~>) :: [Type] -> Type -> Type
(~>) = Fun []
