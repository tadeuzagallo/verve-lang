module AST where

data SourcePos = SourcePos { line :: Int, column :: Int, file :: String }
  deriving (Show, Eq)

data Program = Program [Import] [TopDecl]
  deriving (Show)

data Import = Import { names :: (Maybe [String])
                     , path :: String
                     , alias :: Maybe String
                     } deriving (Show, Eq)

data TopDecl = InterfaceDecl Interface
             | ImplementationDecl Implementation
             | ExternDecl Prototype
             | TypeDecl EnumType
             | ExprDecl Expr
             deriving (Show, Eq)

data EnumType = EnumType { enum_name :: String
                         , type_variables :: (Maybe [String])
                         , constructors :: [TypeConstructor]
                         } deriving (Show, Eq)

data Interface = Interface { interface_name :: String
                           , interface_var :: String
                           , interface_functions :: [InterfaceFunction]
                           } deriving (Show, Eq)

data InterfaceFunction = AbstractFunction Prototype
                       | ConcreteFunction Function
                       deriving (Show, Eq)

data Implementation = Implementation { target_interface :: String
                                     , implementation_type :: Type
                                     , implementation_functions :: [ImplementationFunction]
                                     } deriving (Show, Eq)

data ImplementationFunction = ExternImplementation String
                            | LocalImplementation Function
                            deriving (Show, Eq)

data Literal = Number (Either Integer Double)
             | String String
             | Identifier String
             | List [Expr]
             deriving (Show, Eq)

data FnType = FnType { fn_variables :: (Maybe [String])
                     , parameters :: [Type]
                     , return_type :: Type
                     } deriving (Show, Eq)

data TypeConstructor = TypeContructor { ctor_name :: String
                                      , ctor_arguments :: [Type]
                                      } deriving (Show, Eq)

data Type = BasicType String
         | FunctionType FnType
         | DataType String [Type]
         deriving (Show, Eq)


data Prototype = Prototype { proto_name :: String
                           , signature :: FnType
                           } deriving (Show, Eq)

data Assignment = Assignment { assignee :: Expr
                             , value :: Expr
                             } deriving (Show, Eq)

data Case = Case { pattern :: Pattern
                 , block :: Block
                 } deriving (Show, Eq)

data FunctionParameter = FunctionParameter { param_name :: String, index :: Int
                                           , param_type :: (Maybe Type)
                                           } deriving (Show, Eq)

data Pattern =  Pattern { ctor :: String
                        , bindings :: [String]
                        } deriving (Show, Eq)

data Block = Block [Expr] 
  deriving (Show, Eq)

data Function = Function { fn_name :: String
                         , variables :: Maybe [String]
                         , params :: [FunctionParameter]
                         , ret_type :: Maybe Type
                         , body :: Block
                         } deriving (Show, Eq)

data Expr = Match { match_value :: Expr, cases :: [Case] }
          | If { condition :: Expr, consequent :: Block , alternate :: Maybe Block }
          | Let { assignments :: [Assignment], let_block :: Block } 
          | FunctionExpr Function
          | Call { callee :: Expr, arguments ::  [Expr] }
          | UnaryOp { op :: String, operand :: Expr }
          | BinaryOp { op :: String, lhs :: Expr, rhs :: Expr }
          | LiteralExpr Literal
          deriving (Show, Eq)
