module Absyn where

type Name = String

data Expr
  = Literal Literal
  | Ident Name
  | App { callee :: Expr
        , args :: [Expr] }
  | BinOp { lhs :: Expr
          , op :: Name
          , rhs :: Expr }
  deriving (Show)

data Literal
  = Integer Integer
  | Float Double
  | Char Char
  | String String
  deriving (Show)
