module AST where

data AST = Program [AST] [AST]
         | Import (Maybe [String]) String (Maybe String)
         | Block [AST]
         | Number (Either Integer Double)
         | String String
         | Identifier String
         | BinaryOp String AST AST
         | UnaryOp String AST
         | If { condition :: AST, consequent :: AST , alternate :: Maybe AST }
         | Function { name :: String, params :: [AST], ret_type :: Maybe AST, body :: AST }
         | Call AST [AST]
         | Interface String String [AST]
         | Implementation String AST [AST]
         | EnumType String [String] [AST]
         | TypeContructor String [AST]
         | FunctionType [AST] AST
         | DataType String [AST]
         | BasicType String
         | Prototype String [String] AST
         | List [AST]
         | Let [AST] AST
         | Assignment AST AST
         | FunctionParameter String (Maybe AST)
         | Match { value :: AST, cases :: [AST] }
         | Case { pattern :: AST, block :: AST }
         | Pattern { ctor :: String, bindings :: [String] }
         | Extern AST
         | Virtual AST
         deriving (Show, Eq)
