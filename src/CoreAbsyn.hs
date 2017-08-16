module CoreAbsyn where

import Absyn (Id, Literal, Name)
import Types (Type)

data Expr
  = Void
  | Lit Literal
  | Var (Id Type)
  | App Expr Expr
  | Lam (Id Type) Expr
  | Let [Bind] Expr
  | Match Expr [Case]
  | Type Type
  | Record [Bind]
  deriving (Show)

type Bind = (Id Type, Expr)

type Case = (Pattern, Expr)

data Pattern
  = PatDefault
  | PatLiteral Literal
  | PatVar (Id Type)
  | PatCtor (Id Type) [Pattern]
  deriving (Show)
