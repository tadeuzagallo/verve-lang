{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Typing.Types
  ( Type(..)
  , Intf(..)
  , ConstraintArg(..)
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

import Prelude hiding (concat)

import Typing.State
import Util.PrettyPrint

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
  pprint (TV v _) = str v

newVar :: String -> Tc Var
newVar name = do
  uid <- mkUniqueId
  return $ TV name uid

freshVar :: Var -> Tc Var
freshVar (TV name _) = newVar name

data Type
  = Con String
  | Var Var [Intf]
  | Fun [BoundVar] [Type] Type
  | Rec [(String, Type)]
  | Cls String
  | TyAbs [Var] Type
  | TyApp Type [Type]
  | Top
  | Bot
  | Type
  deriving (Eq)

type BoundVar = (Var, [Intf])

data Intf = Intf String Var [(String, Type)]
  deriving (Eq)

instance Show Intf where
  show (Intf name _ _) = name

instance PrettyPrint Intf where
  pprint (Intf name _ _) = str name

printType :: (Type  -> String) -> (Var -> String) -> (Intf -> String) -> Type -> String
printType p _ _ t@(Con _) = p t
printType p _ _ t@(Var _ _) = p t
printType p _ _ t@(Cls _) = p t
printType _ _ _ Type = "Type"
printType _ _ _ Top = "⊤"
printType _ _ _ Bot = "⊥"

printType f pv pi (Fun gs [v] t2) | v == void =
  printType f pv pi(Fun gs [] t2)

printType f printVar printIntf (Fun gs t1 t2) =
  printf "%s(%s) -> %s"
    (if null gs then "" else printf "∀%s. " (intercalate " " $ map var gs))
    (intercalate ", " $ map (printType f printVar printIntf) t1)
    (printType f printVar printIntf t2)
      where
        var :: BoundVar -> String
        var (v, []) = printVar v
        var (v, [t]) = printf "(%s: %s)" (printVar v) (printIntf t)
        var (v, ts) = printf "(%s: (%s))" (printVar v) (intercalate ", " $ map printIntf ts)

printType f v i (Rec fields) =
  "{" ++ fields' ++ "}"
    where
      fields' = intercalate ", " $ map showField fields
      showField (key, ty) = key ++ ": " ++ printType f v i ty

printType f v i (TyAbs params ty) =
  "∀" ++ (intercalate " " $ map v params)  ++ ". " ++ printType f v i ty

printType f v i (TyApp ty args) =
  printType f v i ty ++ "<" ++ intercalate ", " (map (printType f v i) args) ++ ">"

instance Show Type where
  show (Con t) = t
  show (Var v _) = show v
  show (Cls t) = t
  show t = printType show show show t

instance PrettyPrint Type where
  pprint (Con t) = str t
  pprint (Var v _) = pprint v
  pprint (Cls t) = str t
  pprint Type = str "Type"
  pprint Top = str "⊤"
  pprint Bot = str "⊥"

  pprint (Fun gs [v] t2) | v == void =
    pprint (Fun gs [] t2)

  pprint (Fun gs t1 t2) =
    concat [ ppgs
           , str "(",  ppt1, str ")"
           , str " -> ", pprint t2
           ]
      where
        ppgs =
          if null gs
             then nil
             else concat [ str "∀"
                         , interleave (str " ") (map pprint gs)
                         , str ". "
                         ]

        ppt1 = interleave (str ", ") $ map pprint t1

  pprint (Rec fields) =
    concat [ str "{"
           , fields'
           , str "}"
           ]
      where
        fields' = interleave (str ", ") $ map pprintField fields
        pprintField (key, ty) = concat [ str key,  str ": ", pprint ty]

  pprint (TyAbs params ty) =
    concat [ str "∀"
           , interleave (str " ") (map pprint params)
           , str ". "
           , pprint ty
           ]

  pprint (TyApp ty args) =
    concat [ pprint ty
           , str "<"
           , interleave (str ", ") (map pprint args)
           , str ">"
           ]

instance PrettyPrint BoundVar where
  pprint (v, []) = pprint v
  pprint (v, [t]) =
    concat [ str "("
           , pprint v
           , str ": "
           , pprint t
           , str ")"]
  pprint (v, ts) =
    concat [ str "("
           , pprint v
           , str ": ("
           , interleave (str ", ") $ map pprint ts
           , str "))"]

-- Free Type Variables
fv :: Type -> [Var]
-- No free variables
fv Top = []
fv Bot = []
fv Type = []
fv (Con _) = []
fv (Cls _) = []

-- Var
fv (Var x _) = [x]

-- Combinations
fv (Fun x s r) =
  (foldl union [] (map fv s) `union` fv r) Data.List.\\ map fst x
fv (Rec fields) =
  foldl union [] $ map (fv . snd) fields
fv (TyAbs gen ty) =
  fv ty Data.List.\\ gen
fv (TyApp ty args) =
 foldl union [] (map fv args) `union` fv ty

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

data ConstraintArg
  = CAType Type
  | CABound Type Intf
  | CAPoly Type Intf [ConstraintArg]
  deriving (Show)
