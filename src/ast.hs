module AST where

data AST = Program { imports :: [AST], expressions:: [AST] }
         | Import { names :: (Maybe [String]), path :: String, alias :: (Maybe String) }
         | UnaryOp { op :: String, operand :: AST }
         | BinaryOp { op :: String, lhs :: AST, rhs :: AST }
         | If { condition :: AST, consequent :: AST , alternate :: Maybe AST }
         | Function { name :: String, variables :: Maybe [String], params :: [AST], ret_type :: Maybe AST, body :: AST }
         | Call { callee :: AST, arguments ::  [AST] }
         | Interface { name :: String, generic :: String,  functions :: [AST] }
         | Implementation { name :: String, impl_type :: AST, functions :: [AST] }
         | EnumType { name :: String, variables :: (Maybe [String]), constructors :: [AST] }
         | TypeContructor { name :: String, arguments :: [AST] }
         | FunctionType { variables ::(Maybe [String]), parameters :: [AST], return_type :: AST }
         | DataType { name :: String, arguments :: [AST] }
         | Prototype { name :: String, prototype :: AST }
         | Let { declarations :: [AST], body :: AST }
         | Assignment { lhs :: AST, rhs :: AST }
         | FunctionParameter { name :: String, index :: Int, type' :: (Maybe AST) }
         | Match { value :: AST, cases :: [AST] }
         | Case { pattern :: AST, block :: AST }
         | Pattern { ctor :: String, bindings :: [String] }
         | Block [AST]
         | Number (Either Integer Double)
         | String String
         | Identifier String
         | List [AST]
         | BasicType String
         | Extern AST
         | Virtual AST
         deriving (Show, Eq)
