module AST where

data SourcePos = SourcePos { line :: Int, column :: Int, file :: String }
  deriving (Show)

data Loc id = Loc SourcePos id
  deriving (Show)

data Program id = Program [Import id] [TopDecl id]
  deriving (Show)

data Import id = Import { names :: (Maybe [id])
                        , path :: id
                        , alias :: Maybe id
                        } deriving (Show)

data TopDecl id = InterfaceDecl (Interface id)
                | ImplementationDecl (Implementation id)
                | ExternDecl (Prototype id)
                | TypeDecl (EnumType id)
                | ExprDecl (Expr id)
                deriving (Show)

data EnumType id = EnumType { enum_name :: id
                            , type_variables :: (Maybe [String])
                            , constructors :: [TypeConstructor id]
                            } deriving (Show)

data Interface id = Interface { interface_name :: id
                              , interface_var :: id
                              , interface_functions :: [InterfaceFunction id ]
                              } deriving (Show)

data InterfaceFunction id = AbstractFunction (Prototype id)
                          | ConcreteFunction (Function id)
                          deriving (Show)

data Implementation id = Implementation { target_interface :: Loc id
                                        , implementation_type :: Type id
                                        , implementation_functions :: [ImplementationFunction id]
                                        } deriving (Show)

data ImplementationFunction id = ExternImplementation (Loc id)
                               | LocalImplementation (Function id)
                               deriving (Show)

data Literal
  = Number (Either Integer Double)
  | String String
  deriving (Show)

data FnType id = FnType { fn_variables :: (Maybe [id])
                        , parameters :: [Type id]
                        , return_type :: Type id
                        } deriving (Show)

data TypeConstructor id = TypeConstructor { ctor_name :: id
                                          , ctor_arguments :: [Type id]
                                          } deriving (Show)

data Type id = BasicType (Loc id)
             | FunctionType (FnType id)
             | DataType (Loc id) [Type id]
             deriving (Show)


data Prototype id = Prototype { proto_name :: id
                              , signature :: FnType id
                              } deriving (Show)

data Assignment id = Assignment { assignee :: Expr id
                                , value :: Expr id
                                } deriving (Show)

data Case id = Case { pattern :: Pattern id
                    , block :: Block id
                    } deriving (Show)

data FunctionParameter id = FunctionParameter { param_name :: (Loc id)
                                              , index :: Int
                                              , param_type :: Maybe (Type id)
                                              } deriving (Show)

data Pattern id =  Pattern { ctor :: id
                           , bindings :: [id]
                           } deriving (Show)

data Block id = Block [Expr id] 
  deriving (Show)

data Function id = Function { fn_name :: Loc id
                            , variables :: Maybe [id]
                            , params :: [FunctionParameter id]
                            , ret_type :: Maybe (Type id)
                            , body :: (Block id)
                            } deriving (Show)

data Expr id = Match { match_value :: Expr id, cases :: [Case id] }
             | If { condition :: Expr id, consequent :: Block id, alternate :: Maybe (Block id) }
             | Let { assignments :: [Assignment id], let_block :: Block id } 
             | FunctionExpr (Function id)
             | Call { callee :: Expr id, arguments ::  Loc [Expr id] }
             | UnaryOp { op :: Loc id, operand :: Expr id}
             | BinaryOp { op :: Loc id, lhs :: Expr id, rhs :: Expr id}
             | Var (Loc id)
             | Arg (Loc id) Int
             | LiteralExpr Literal
             | List [Expr id]
             deriving (Show)
