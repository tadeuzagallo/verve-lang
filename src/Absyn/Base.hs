module Absyn.Base where
  
import Absyn.Meta

data BaseModule a b = Module
  { stmts :: [BaseStmt a b]
  }

data BaseStmt a b
  = FnStmt (BaseFunction a b)
  | Expr (BaseExpr a b)
  | Enum a [Name] [BaseDataCtor a b]
  | Let a (BaseExpr a b)
  | Class { className :: a
          , classVars :: [BaseParam b]
          , classMethods :: [BaseFunction a b]
          }
  | Operator { opAssoc :: Associativity
             , opPrec :: Precedence
             , opGenerics :: BaseGenerics b
             , opLhs :: BaseParam b
             , opName :: a
             , opRhs :: BaseParam b
             , opRetType :: b
             , opBody :: [BaseStmt a b]
             }
  | Interface { intfName :: a
              , intfParam :: Name
              , intfMethods :: [BaseFunctionDecl a b]}
  | Implementation { implName :: a
                   , implGenerics :: BaseGenerics b
                   , implType :: b
                   , implMethods :: [BaseFunction a b]}
   deriving (Show)

type BaseDataCtor a b = (a, Maybe [b])
type BaseParam b = (Name, b)
type BaseGenerics b = [(Name, [b])]

data BaseFunction a b = Function
  { name :: a
  , generics :: BaseGenerics b
  , params :: [BaseParam b]
  , retType :: b
  , body :: [BaseStmt a b]
  } deriving (Show)

data BaseFunctionDecl a b = FunctionDecl
  { fnDeclName :: a
  , fnDeclGenerics :: BaseGenerics b
  , fnDeclParams :: [BaseParam b]
  , fnDeclRetType :: b
  } deriving (Show)

data BaseExpr a b
  = Literal Literal
  | Ident a
  | VoidExpr -- workaround, this can't be expressed in source code
  | ParenthesizedExpr (BaseExpr a b)
  | Match { expr :: BaseExpr a b
          , cases :: [BaseCase a b]
          }
  | If { ifCond :: BaseExpr a b
       , ifBody :: [BaseStmt a b]
       , ifElseBody :: [BaseStmt a b]
       }
  | Call { callee :: BaseExpr a b
         , constraintArgs :: [(b, b)]
         , typeArgs :: [b]
         , args :: [BaseExpr a b]
         }
  | BinOp { opConstraintArgs :: [(b, b)]
          , opTypeArgs :: [b]
          , lhs :: BaseExpr a b
          , op :: a
          , rhs :: BaseExpr a b
          }
  | Record [(a, BaseExpr a b)]
  | List [BaseExpr a b]
  | FieldAccess (BaseExpr a b) b a
  | FnExpr (BaseFunction a b)
  deriving (Show)

data BaseCase a b = Case { pattern :: BasePattern a
                         , caseBody :: [BaseStmt a b]
                         } deriving (Show)

data BasePattern a
  = PatDefault
  | PatLiteral Literal
  | PatVar a
  | PatCtor a [BasePattern a]
  deriving (Show)
