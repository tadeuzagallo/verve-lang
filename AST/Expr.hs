module AST.Expr where

import AST.Literal

data Bind =
  BLet String Stmt
  | BStmt Stmt
  deriving (Show)

data Stmt =
  SMatch
  | SIf
  | SExpr Expr
  deriving (Show)

data Expr =
  EFn Fn
  | ECall Expr [Expr]
  | EVar String
  | EArg
  | EBinop String Expr Expr
  | EUnop String Expr
  | ELiteral Literal
  | EStmt Stmt
  deriving (Show)

data Fn = Fn [String] Type [Expr]
  deriving Show

data Type = TBasic String
  deriving Show
