module Absyn where

import Types

type Name = String

data Id = Id Name Type
  deriving (Show)

data Module a = Module
  { stmts :: [Stmt a]
  }

data Stmt a
  = FnStmt (Function a)
  | Expr (Expr a)
  | Enum a [DataCtor a]

type DataCtor a = (a, Maybe [Type])

data Function a = Function
  { name :: a
  , generics :: [Name]
  , params :: [TypedName]
  , retType :: Type
  , body :: [Stmt a]
  }

type TypedName = (Name, Type)

data Expr a
  = Literal Literal
  | Ident a
  | VoidExpr -- workaround, this can't be expressed in source code
  | Match { expr :: Expr a
          , cases :: [Case a]
          }
  | App { callee :: Expr a
        , types :: [Type]
        , args :: [Expr a] }
  | BinOp { lhs :: Expr a
          , op :: String
          , rhs :: Expr a }
  deriving (Show)

data Case a = Case { pattern :: Pattern a
                   , caseBody :: Expr a
                   } deriving (Show)

data Pattern a
  = PatDefault
  | PatLiteral Literal
  | PatVar a
  | PatCtor a [Pattern a]
  deriving (Show)

data Literal
  = Integer Integer
  | Float Double
  | Char Char
  | String String
  deriving (Eq)

instance Show Literal where
  show (Integer i) = show i
  show (Float i) = show i
  show (Char i) = show i
  show (String i) = show i
