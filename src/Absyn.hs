module Absyn where

type Name = String

data UnresolvedType
  = UTName Name
  | UTApp UnresolvedType [UnresolvedType]
  | UTArrow [UnresolvedType] UnresolvedType
  | UTRecord [(Name, UnresolvedType)]
  | UTVoid
  | UTPlaceholder
  deriving (Show)

type Id b = (Name, b)

data Module a b = Module
  { stmts :: [Stmt a b]
  }

data Stmt a b
  = FnStmt (Function a b)
  | Expr (Expr a b)
  | Enum a [Name] [DataCtor a b]
  | Let a (Expr a b)
  | Class { className :: a
          , classVars :: [(Name, b)]
          , classMethods :: [Function a b]
          }
  | Operator { opAssoc :: Associativity
             , opPrec :: Precedence
             , opGenerics :: [(Name, [b])]
             , opLhs :: Id b
             , opName :: a
             , opRhs :: Id b
             , opRetType :: b
             , opBody :: [Stmt a b]
             }
  | Interface { intfName :: a
              , intfParam :: Name
              , intfMethods :: [FunctionDecl a b]}
  | Implementation { implName :: a
                   , implType :: b
                   , implMethods :: [Function a b]}
   deriving (Show)

type DataCtor a b = (a, Maybe [b])

data Associativity
  = AssocNone
  | AssocLeft
  | AssocRight

instance Show Associativity where
  show AssocNone = "none"
  show AssocLeft = "left"
  show AssocRight = "right"

defaultAssoc :: Associativity
defaultAssoc = AssocLeft

data Precedence
  = PrecHigher Name
  | PrecLower Name
  | PrecEqual Name
  | PrecValue Integer

instance Show Precedence where
  show (PrecHigher name) = "higher(" ++ name ++ ")"
  show (PrecLower name) = "lower(" ++ name ++ ")"
  show (PrecEqual name) = "equal(" ++ name ++ ")"
  show (PrecValue n) = show n

defaultPrec :: Precedence
defaultPrec = PrecValue 50

data Function a b = Function
  { name :: a
  , generics :: [(Name, [b])]
  , params :: [Id b]
  , retType :: b
  , body :: [Stmt a b]
  } deriving (Show)

data FunctionDecl a b = FunctionDecl
  { fnDeclName :: a
  , fnDeclGenerics :: [(Name, [b])]
  , fnDeclParams :: [Id b]
  , fnDeclRetType :: b
  } deriving (Show)

data Expr a b
  = Literal Literal
  | Ident a
  | VoidExpr -- workaround, this can't be expressed in source code
  | ParenthesizedExpr (Expr a b)
  | Match { expr :: Expr a b
          , cases :: [Case a b]
          }
  | If { ifCond :: Expr a b
       , ifBody :: [Stmt a b]
       , ifElseBody :: [Stmt a b]
       }
  | Call { callee :: Expr a b
         , constraintArgs :: [(b, b)]
         , typeArgs :: [b]
         , args :: [Expr a b]
         }
  | BinOp { opConstraintArgs :: [(b, b)]
          , opTypeArgs :: [b]
          , lhs :: Expr a b
          , op :: a
          , rhs :: Expr a b
          }
  | Record [(a, Expr a b)]
  | List [Expr a b]
  | FieldAccess (Expr a b) b a
  | FnExpr (Function a b)
  deriving (Show)

data Case a b = Case { pattern :: Pattern a
                     , caseBody :: [Stmt a b]
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
