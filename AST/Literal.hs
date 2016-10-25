module AST.Literal where

data Literal =
  LNum (Either Integer Double)
  deriving (Show)
