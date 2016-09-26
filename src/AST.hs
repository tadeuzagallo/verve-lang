module AST where

data SourcePos = SourcePos { line :: Int, column :: Int, file :: String }
  deriving (Show, Eq)

data Program id = Program [Import id] [TopDecl id]
  deriving (Show)

data Import id = Import { names :: (Maybe [id])
                        , path :: id
                        , alias :: Maybe id
                        } deriving (Show, Eq)

data TopDecl id = InterfaceDecl (Interface id)
                | ImplementationDecl (Implementation id)
                | ExternDecl (Prototype id)
                | TypeDecl (EnumType id)
                | ExprDecl (Expr id)
                deriving (Show, Eq)

data EnumType id = EnumType { enum_name :: id
                            , type_variables :: (Maybe [id])
                            , constructors :: [TypeConstructor id]
                            } deriving (Show, Eq)

data Interface id = Interface { interface_name :: id
                              , interface_var :: id
                              , interface_functions :: [InterfaceFunction id ]
                              } deriving (Show, Eq)

data InterfaceFunction id = AbstractFunction (Prototype id)
                          | ConcreteFunction (Function id)
                          deriving (Show, Eq)

data Implementation id = Implementation { target_interface :: id
                                        , implementation_type :: Type id
                                        , implementation_functions :: [ImplementationFunction id]
                                        } deriving (Show, Eq)

data ImplementationFunction id = ExternImplementation id
                               | LocalImplementation (Function id)
                               deriving (Show, Eq)

data Literal id = Number (Either Integer Double)
                | String id
                | Identifier id
                | List [Expr id]
                deriving (Show, Eq)

data FnType id = FnType { fn_variables :: (Maybe [id])
                        , parameters :: [Type id]
                        , return_type :: Type id
                        } deriving (Show, Eq)

data TypeConstructor id = TypeContructor { ctor_name :: id
                                         , ctor_arguments :: [Type id]
                                         } deriving (Show, Eq)

data Type id = BasicType id
             | FunctionType (FnType id)
             | DataType id [Type id]
             deriving (Show, Eq)


data Prototype id = Prototype { proto_name :: id
                              , signature :: FnType id
                              } deriving (Show, Eq)

data Assignment id = Assignment { assignee :: Expr id
                                , value :: Expr id
                                } deriving (Show, Eq)

data Case id = Case { pattern :: Pattern id
                    , block :: Block id
                    } deriving (Show, Eq)

data FunctionParameter id = FunctionParameter { param_name :: id
                                              , index :: Int
                                              , param_type :: Maybe (Type id)
                                              } deriving (Show, Eq)

data Pattern id =  Pattern { ctor :: id
                           , bindings :: [id]
                           } deriving (Show, Eq)

data Block id = Block [Expr id] 
  deriving (Show, Eq)

data Function id = Function { fn_name :: id
                            , variables :: Maybe [id]
                            , params :: [FunctionParameter id]
                            , ret_type :: Maybe (Type id)
                            , body :: (Block id)
                            } deriving (Show, Eq)

data Expr id = Match { match_value :: Expr id, cases :: [Case id] }
             | If { condition :: Expr id, consequent :: Block id, alternate :: Maybe (Block id) }
             | Let { assignments :: [Assignment id], let_block :: Block id } 
             | FunctionExpr (Function id)
             | Call { callee :: Expr id, arguments ::  [Expr id] }
             | UnaryOp { op :: id, operand :: Expr id}
             | BinaryOp { op :: id, lhs :: Expr id, rhs :: Expr id}
             | LiteralExpr (Literal id)
             deriving (Show, Eq)
