module AST.Literal where

data Literal =
    LNum (Either Integer Double)
  | LStr String
  deriving (Show)
