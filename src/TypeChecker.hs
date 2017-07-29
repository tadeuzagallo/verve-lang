module TypeChecker
  ( infer
  , Type
  ) where

import Absyn

data TypeError =
  TypeError
  deriving (Show)

data Type =
  Con String

instance Show Type where
  show (Con t) = t

type InferResult = Either TypeError Type

infer :: Expr -> InferResult
infer expr = i_expr expr

i_expr :: Expr -> InferResult
i_expr (Literal lit) = i_lit lit

i_lit :: Literal -> InferResult
i_lit (Integer x) = return $ Con "Int"
i_lit (Float x) = return $ Con "Float"
i_lit (Char x) = return $ Con "Char"
i_lit (String x) = return $ Con "String"
