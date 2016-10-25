module AST where

data AST = AProgram [Decl]
  deriving (Show)

data Decl =
  DExpr Expr
  deriving (Show)

data Expr =
  ELiteral Literal
  deriving (Show)

data Literal =
  LNum (Either Integer Double)
  deriving (Show)
