module Absyn where

import Types

type Name = String

data Module = Module
  { stmts :: [Stmt]
  }

data Stmt
  = FnStmt Function
  | Expr Expr

data Function = Function
  { name :: Name
  , params :: [TypedName]
  , retType :: Type
  , body :: [Stmt]
  }

data TypedName =
  TypedName Name
            Type

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
