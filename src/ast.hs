module AST where

data SourcePos = SourcePos { line :: Int, column :: Int, file :: String }
  deriving (Show, Eq)

data AST = Program { pos :: SourcePos, imports :: [AST], expressions:: [AST] }
         | Import { pos :: SourcePos, names :: (Maybe [String]), path :: String, alias :: (Maybe String) }
         | UnaryOp { pos :: SourcePos, op :: String, operand :: AST }
         | BinaryOp { pos :: SourcePos, op :: String, lhs :: AST, rhs :: AST }
         | If { pos :: SourcePos, condition :: AST, consequent :: AST , alternate :: Maybe AST }
         | Function { pos :: SourcePos, name :: String, variables :: Maybe [String], params :: [AST], ret_type :: Maybe AST, body :: AST }
         | Call { pos :: SourcePos, callee :: AST, arguments ::  [AST] }
         | Interface { pos :: SourcePos, name :: String, variable :: String,  functions :: [AST] }
         | Implementation { pos :: SourcePos, name :: String, impl_type :: AST, functions :: [AST] }
         | EnumType { pos :: SourcePos, name :: String, variables :: (Maybe [String]), constructors :: [AST] }
         | TypeContructor { pos :: SourcePos, name :: String, arguments :: [AST] }
         | FunctionType { pos :: SourcePos, variables ::(Maybe [String]), parameters :: [AST], return_type :: AST }
         | DataType { pos :: SourcePos, name :: String, arguments :: [AST] }
         | Prototype { pos :: SourcePos, name :: String, prototype :: AST }
         | Let { pos :: SourcePos, declarations :: [AST], body :: AST }
         | Assignment { pos :: SourcePos, lhs :: AST, rhs :: AST }
         | FunctionParameter { pos :: SourcePos, name :: String, index :: Int, type' :: (Maybe AST) }
         | Match { pos :: SourcePos, value :: AST, cases :: [AST] }
         | Case { pos :: SourcePos, pattern :: AST, block :: AST }
         | Pattern { pos :: SourcePos, ctor :: String, bindings :: [String] }
         | Block { pos :: SourcePos, nodes :: [AST] }
         | Number { pos :: SourcePos, num_value :: (Either Integer Double) }
         | String { pos :: SourcePos, str_value :: String }
         | Identifier { pos :: SourcePos, name :: String }
         | List { pos :: SourcePos, items :: [AST] }
         | BasicType { pos :: SourcePos, type_name :: String }
         | Extern { pos :: SourcePos, prototype :: AST }
         | Virtual { pos :: SourcePos, prototype :: AST }
         deriving (Show, Eq)
