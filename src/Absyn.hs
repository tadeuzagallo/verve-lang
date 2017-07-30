module Absyn where

import Types

data Name
  = Global String
  | Local Int
  deriving (Show)

data Module = Module
  { stmts :: [Stmt]
  }

data Stmt
  = FnStmt Function
  | Expr Expr

data Function = Function
  { name :: String
  , params :: [TypedName]
  , retType :: Type
  , body :: [Stmt]
  }

data TypedName =
  TypedName String
            Type

data Expr
  = Literal Literal
  | Ident Name
  | App { callee :: Expr
        , args :: [Expr] }
  | BinOp { lhs :: Expr
          , op :: String
          , rhs :: Expr }
  deriving (Show)

data Literal
  = Integer Integer
  | Float Double
  | Char Char
  | String String
  deriving (Show)
