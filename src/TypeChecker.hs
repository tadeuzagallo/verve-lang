module TypeChecker
  ( infer
  , Type
  ) where

import Absyn

import Data.List (intercalate)
import Text.Printf (printf)

data TypeError =
  UnknownVariable String
  deriving (Show)

data Type
  = Con String
  | Arr [Type]
        Type

int :: Type
int = Con "Int"

float :: Type
float = Con "Float"

char :: Type
char = Con "Char"

string :: Type
string = Con "String"

void :: Type
void = Con "Void"

instance Show Type where
  show (Con t) = t
  show (Arr t1 t2) =
    printf "(%s) -> %s" (intercalate ", " $ map show t1) (show t2)

type InferResult = Either TypeError Type

newtype Ctx =
  Ctx [(Name, Type)]

getType :: Name -> Ctx -> Maybe Type
getType n (Ctx ctx) = lookup n ctx

defaultCtx :: Ctx
defaultCtx =
  Ctx [("int_print", Arr [int] void), ("int_add", Arr [int, int] int)]

infer :: Expr -> InferResult
infer expr = i_expr defaultCtx expr

i_expr :: Ctx -> Expr -> InferResult
i_expr _ (Literal lit) = i_lit lit
i_expr ctx (Ident i) =
  case getType i ctx of
    Nothing -> Left $ UnknownVariable i
    Just t -> return t

i_lit :: Literal -> InferResult
i_lit (Integer _) = return int
i_lit (Float _) = return float
i_lit (Char _) = return char
i_lit (String _) = return string
