module AST.Decl where

import AST.Expr

data Decl =
    DExtern
  | DInterface
  | DImplementation
  | DType
  | DBind Bind
  deriving (Show)
