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
  | EBinop
  | EUnop
  | ELiteral Literal
  deriving (Show)
