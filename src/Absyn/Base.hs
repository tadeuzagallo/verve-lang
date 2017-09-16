module Absyn.Base where

import Absyn.Meta
import Typing.Types (ConstraintArg)

data BaseModule a b c = Module
  { imports :: [Import]
  , stmts :: [BaseStmt a b c]
  }

data BaseStmt a b c
  = FnStmt (BaseFunction a b c)
  | Expr (BaseExpr a b c)
  | Enum a [Name] [BaseDataCtor a b]
  | Let a (BaseExpr a b c)
  | Class { className :: a
          , classVars :: [BaseParam b]
          , classMethods :: [BaseFunction a b c]
          }
  | Operator { opAssoc :: Associativity
             , opPrec :: Precedence
             , opGenerics :: BaseGenerics c
             , opLhs :: BaseParam b
             , opName :: a
             , opRhs :: BaseParam b
             , opRetType :: b
             , opBody :: [BaseStmt a b c]
             }
  | Interface { intfName :: a
              , intfParam :: Name
              , intfMethods :: [BaseFunctionDecl a b c]}
  | Implementation { implName :: a
                   , implGenerics :: BaseGenerics c
                   , implType :: b
                   , implMethods :: [BaseFunction a b c]}
  | TypeAlias { aliasName :: Name
              , aliasVars :: [Name]
              , aliasType :: b
              }
   deriving (Show)

type BaseDataCtor a b = (a, Maybe [b])
type BaseParam b = (Name, b)
type BaseGenerics c = [(Name, [c])]

data BaseFunction a b c = Function
  { name :: a
  , generics :: BaseGenerics c
  , params :: [BaseParam b]
  , retType :: b
  , body :: [BaseStmt a b c]
  } deriving (Show)

data BaseFunctionDecl a b c = FunctionDecl
  { fnDeclName :: a
  , fnDeclGenerics :: BaseGenerics c
  , fnDeclParams :: [BaseParam b]
  , fnDeclRetType :: b
  } deriving (Show)

data BaseExpr a b c
  = Literal Literal
  | Ident [Name] b
  | VoidExpr -- workaround, this can't be expressed in source code
  | ParenthesizedExpr (BaseExpr a b c)
  | Match { expr :: BaseExpr a b c
          , cases :: [BaseCase a b c]
          }
  | If { ifCond :: BaseExpr a b c
       , ifBody :: [BaseStmt a b c]
       , ifElseBody :: [BaseStmt a b c]
       }
  | Call { callee :: BaseExpr a b c
         , constraintArgs :: [ConstraintArg]
         , typeArgs :: [b]
         , args :: [BaseExpr a b c]
         }
  | BinOp { opConstraintArgs :: [ConstraintArg]
          , opTypeArgs :: [b]
          , lhs :: BaseExpr a b c
          , op :: a
          , rhs :: BaseExpr a b c
          }
  | Record [(a, BaseExpr a b c)]
  | List b [BaseExpr a b c]
  | FieldAccess (BaseExpr a b c) b a
  | FnExpr (BaseFunction a b c)
  deriving (Show)

data BaseCase a b c = Case { pattern :: BasePattern a
                           , caseBody :: [BaseStmt a b c]
                           } deriving (Show)

data BasePattern a
  = PatDefault
  | PatLiteral Literal
  | PatVar a
  | PatRecord [(a, BasePattern a)]
  | PatCtor a [BasePattern a]
  deriving (Show)
