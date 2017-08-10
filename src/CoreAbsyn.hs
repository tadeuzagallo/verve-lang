module CoreAbsyn where

import Absyn (Id, Literal, Name)
import Types

data Expr
  = Void
  | Lit Literal
  | Var Id
  | App Expr Expr
  | Lam Id Expr
  | Let [Bind] Expr
  | Match Expr [Case]
  | Type Type
  deriving (Show)

type Bind = (Id, Expr)

type Case = (Pattern, Expr)

data Pattern
  = PatDefault
  | PatLiteral Literal
  | PatVar Id
  | PatCtor Id [Pattern]
  deriving (Show)
