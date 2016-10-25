module AST.Expr where

import AST.Literal

data Expr =
  EMatch
  | EIf
  | ELet
  | EFn
  | ECall
  | EVar
  | EArg
  | EBinop String Expr Expr
  | EUnop String Expr
  | ELiteral Literal
  deriving (Show)
