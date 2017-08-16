module Absyn where

import Types (Type)

type Name = String

newtype UnresolvedType = UnresolvedType Type
  deriving (Show)

type Id b = (Name, b)

data Module a b = Module
  { stmts :: [Stmt a b]
  }

data Stmt a b
  = FnStmt (Function a b)
  | Expr (Expr a b)
  | Enum a [DataCtor a b]
  | Let a (Expr a b)
  | Operator { opGenerics :: [Name]
             , opLhs :: Id b
             , opName :: a
             , opRhs :: Id b
             , opRetType :: b
             , opBody :: [Stmt a b]
             }

type DataCtor a b = (a, Maybe [b])

data Function a b = Function
  { name :: a
  , generics :: [Name]
  , params :: [Id b]
  , retType :: b
  , body :: [Stmt a b]
  }

data Expr a b
  = Literal Literal
  | Ident a
  | VoidExpr -- workaround, this can't be expressed in source code
  | Match { expr :: Expr a b
          , cases :: [Case a b]
          }
  | App { callee :: Expr a b
        , types :: [b]
        , args :: [Expr a b]
        }
  | BinOp { lhs :: Expr a b
          , op :: a
          , rhs :: Expr a b
          }
  | Record [(a, Expr a b)]
  deriving (Show)

data Case a b = Case { pattern :: Pattern a
                     , caseBody :: Expr a b
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
