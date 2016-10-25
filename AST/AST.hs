module AST.AST where

import AST.Decl

data AST = AProgram [Decl]
  deriving (Show)
