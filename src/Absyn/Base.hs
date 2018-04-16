module Absyn.Base where

import Absyn.Meta
import Typing.Types (ConstraintArg)

data BaseModule a b = Module
  { imports :: [Import]
  , stmts :: [BaseStmt a b]
  }

data BaseStmt a b
  = Decl (BaseDecl a b)
  | Expr (BaseExpr a b)
  deriving (Show)

data BaseDecl a b
  = FnStmt (BaseFunction a b)
  | Enum a [Name] [BaseDataCtor a b]
  | Let (Name, b) (BaseExpr a b)
  | Class { className :: a
          , classVars :: [BaseParam b]
          , classMethods :: [BaseFunction a b]
          }
  | Operator { opAssoc :: Associativity
             , opPrec :: Precedence
             , opGenerics :: BaseGenerics
             , opLhs :: BaseParam b
             , opName :: a
             , opRhs :: BaseParam b
             , opRetType :: b
             , opBody :: [BaseStmt a b]
             }
  | Interface { intfName :: a
              , intfParam :: Name
              , intfMethods :: [BaseInterfaceItem a b]}
  | Implementation { implIntf :: a
                   , implGenerics :: BaseGenerics
                   , implType :: b
                   , implMethods :: [BaseImplementationItem a b]}
  | TypeAlias { aliasName :: Name
              , aliasVars :: [Name]
              , aliasType :: b
              }
   deriving (Show)

data BaseInterfaceItem a b
  = IntfVar (Name, b)
  | IntfOperator { intfOpAssoc :: Associativity
                 , intfOpPrec :: Precedence
                 , intfOpLhs :: b
                 , intfOpName :: a
                 , intfOpRhs :: b
                 , intfOpRetType :: b
                 }

  deriving (Show)

data BaseImplementationItem a b
  = ImplVar (a, BaseExpr a b)
  | ImplFunction { implName :: a
                 , implParams :: [Name]
                 , implBody :: [BaseStmt a b]
                 }
  | ImplOperator { implOpLhs :: Name
                 , implOpName :: a
                 , implOpRhs :: Name
                 , implOpBody :: [BaseStmt a b]
                 }
 deriving (Show)

type BaseDataCtor a b = (a, Maybe [b])
type BaseParam b = (Name, b)
type BaseGenerics = [(Name, [String])]

data BaseFunction a b = Function
  { name :: a
  , generics :: BaseGenerics
  , params :: [BaseParam b]
  , retType :: b
  , body :: [BaseStmt a b]
  } deriving (Show)

data BaseExpr a b
  = Literal Literal
  | Ident [Name] b
  | ParenthesizedExpr (BaseExpr a b)
  | Match { expr :: BaseExpr a b
          , cases :: [BaseCase a b]
          }
  | If { ifCond :: BaseExpr a b
       , ifBody :: [BaseStmt a b]
       , ifElseBody :: [BaseStmt a b]
       }
  | Call { callee :: BaseExpr a b
         , constraintArgs :: [ConstraintArg]
         , typeArgs :: [b]
         , args :: [BaseExpr a b]
         }
  | BinOp { opConstraintArgs :: [ConstraintArg]
          , opTypeArgs :: [b]
          , lhs :: BaseExpr a b
          , op :: a
          , rhs :: BaseExpr a b
          }
  | Record [(a, BaseExpr a b)]
  | List b [BaseExpr a b]
  | FieldAccess (BaseExpr a b) b a
  | FnExpr (BaseFunction a b)
  | Negate [ConstraintArg] (BaseExpr a b)

  -- Expressions that can only be generated by the compiler
  | VoidExpr
  | TypeCall (BaseExpr a b) [ConstraintArg]
  deriving (Show)

data BaseCase a b = Case { pattern :: BasePattern a
                           , caseBody :: [BaseStmt a b]
                           } deriving (Show)

data BasePattern a
  = PatDefault
  | PatLiteral Literal
  | PatVar a
  | PatRecord [(a, BasePattern a)]
  | PatList [BasePattern a] (BasePatternRest a)
  | PatCtor a [BasePattern a]
  deriving (Show)

data BasePatternRest a
  = NoRest
  | DiscardRest
  | NamedRest a
  deriving (Show)
