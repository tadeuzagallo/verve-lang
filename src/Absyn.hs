module Absyn where

import Types

type Name = String

data Module a = Module
  { stmts :: [Stmt a]
  }

data Stmt a
  = FnStmt (Function a)
  | Expr (Expr a)

data Function a = Function
  { name :: String
  , params :: [TypedName a]
  , retType :: Type
  , body :: [Stmt a]
  }

data TypedName a =
  TypedName a
            Type

data Expr a
  = Literal Literal
  | Ident a
  | VoidExpr -- workaround, this can't be expressed in source code
  | App { callee :: Expr a
        , args :: [Expr a] }
  | BinOp { lhs :: Expr a
          , op :: String
          , rhs :: Expr a }
  deriving (Show)

data Literal
  = Integer Integer
  | Float Double
  | Char Char
  | String String

instance Show Literal where
  show (Integer i) = show i
  show (Float i) = show i
  show (Char i) = show i
  show (String i) = show i
