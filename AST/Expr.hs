module AST.Expr where

import AST.Literal

data Expr =
  EMatch
  | EIf
  | ELet
  | EFn Fn
  | ECall
  | EVar
  | EArg
  | EBinop String Expr Expr
  | EUnop String Expr
  | ELiteral Literal
  deriving (Show)

data Fn = Fn [String] Type [Expr]
  deriving Show

data Type = TBasic String
  deriving Show
