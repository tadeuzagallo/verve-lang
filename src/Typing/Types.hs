module Typing.Types
  ( Type(..)
  , Var()
  , BoundVar
  , newVar
  , freshVar
  , fv

  -- BASE TYPES
  , bool
  , int
  , float
  , char
  , string
  , void
  , list
  , (~>)

  -- HELPERS FOR WRITING BUILTIN TYPES
  , FakeVar(..)
  , tyvar

  -- TYPE HOLE HELPERS
  , mkHole
  , isHole
  ) where

import PrettyPrint
import Typing.State

import Data.List (intercalate, union)
import Text.Printf (printf)

import qualified Data.List ((\\))

data Var = TV String Int
  deriving (Eq)

instance Ord Var where
  compare (TV x m) (TV y n) =
    case compare x y of
      EQ -> compare m n
      ord -> ord

instance Show Var where
  show (TV v _) = v

instance PrettyPrint Var where
  ppr (TV v _) = pprName v

newVar :: String -> Tc Var
newVar name = do
  uid <- mkUniqueId
  return $ TV name uid

freshVar :: Var -> Tc Var
freshVar (TV name _) = newVar name

type BoundVar = (Var, [Type])

data Type
  = Con String
  | Var Var [Type]
  | Fun [BoundVar] [Type] Type
  | Rec [(String, Type)]
  | Cls String [(String, Type)]
  | TyAbs [Var] Type
  | TyApp Type [Type]
  | Intf String Var [(String, Type)]
  | Top
  | Bot
  | Type
  deriving (Eq)

printType :: (Type  -> String) -> (Var -> String) -> Type -> String
printType p _ t@(Con _) = p t
printType p _ t@(Var _ _) = p t
printType p _ t@(Cls _ _) = p t
printType p _ t@(Intf _ _ _) = p t
printType _ _ Type = "Type"
printType _ _ Top = "⊤"
printType _ _ Bot = "⊥"

printType f pv (Fun gs [v] t2) | v == void =
  printType f pv (Fun gs [] t2)

printType f printVar (Fun gs t1 t2) =
  printf "%s(%s) -> %s"
    (if null gs then "" else printf "∀%s. " (intercalate " " $ map var gs))
    (intercalate ", " $ map (printType f printVar) t1)
    (printType f printVar t2)
      where
        var :: BoundVar -> String
        var (v, []) = printVar v
        var (v, [t]) = printf "(%s: %s)" (printVar v) (printType f printVar t)
        var (v, ts) = printf "(%s: (%s))" (printVar v) (intercalate ", " $ map (printType f printVar) ts)

printType f v (Rec fields) =
  "{" ++ fields' ++ "}"
    where
      fields' = intercalate ", " $ map showField fields
      showField (key, ty) = key ++ ": " ++ printType f v ty

printType f v (TyAbs params ty) =
  "∀" ++ (intercalate " " $ map v params)  ++ "." ++ printType f v ty

printType f v (TyApp ty args) =
  printType f v ty ++ "<" ++ intercalate ", " (map (printType f v) args) ++ ">"

instance Show Type where
  show (Con t) = t
  show (Var v _) = show v
  show (Cls t _) = t
  show (Intf t _ _) = t
  show t = printType show show t

instance PrettyPrint Type where
  ppr (Con t) = pprName t
  ppr (Var v _) = ppr v
  ppr (Cls t _) = pprName t
  ppr (Intf t _ _) = pprName t
  ppr t = printType ppr ppr t

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
  (foldl union [] (map fv s) `union` fv r) Data.List.\\ map fst x
fv (Rec fields) =
  foldl union [] $ map (fv . snd) fields
fv (Cls _ fields) =
  foldl union [] $ map (fv . snd) fields
fv (TyAbs gen ty) =
  fv ty Data.List.\\ gen
fv (TyApp ty args) =
 foldl union [] (map fv args) `union` fv ty
fv (Intf _ param methods) =
  (foldl union [] $ map (fv . snd) methods) Data.List.\\ [param]

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

(~>) :: [Type] -> Type -> Type
(~>) = Fun []

data FakeVar
  = T
  deriving (Show)

tyvar :: FakeVar -> Var
tyvar v = TV (show v) (-1)

holeVar :: Var
holeVar = TV "#hole" (-42)

mkHole :: BoundVar -> Type
mkHole (_, bounds) = Var holeVar bounds

isHole :: Type -> Bool
isHole (Var tv _) | tv == holeVar = True
isHole _ = False
