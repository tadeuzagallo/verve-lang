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
  | VoidExpr -- workaround, this can't be expressed in source code
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

instance Show Literal where
  show (Integer i) = show i
  show (Float i) = show i
  show (Char i) = show i
  show (String i) = show i
