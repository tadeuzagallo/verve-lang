module TypeChecker
  ( infer
  ) where

import Absyn
import Types

import Control.Monad (foldM, when)
import Data.List (intercalate)
import Text.Printf (printf)

data TypeError
  = UnknownVariable String
  | ArityMismatch
  | TypeError { expected :: Type
              , actual :: Type }
  deriving (Show)

instance Show Type where
  show (Con t) = t
  show (Arr t1 t2) =
    printf "(%s) -> %s" (intercalate ", " $ map show t1) (show t2)

type InferResult = Either TypeError Type

type InferResultT = Either TypeError

type CheckResult = Either TypeError ()

newtype Ctx =
  Ctx [(String, Type)]

getType :: String -> Ctx -> Maybe Type
getType n (Ctx ctx) = lookup n ctx

addType :: Ctx -> (String, Type) -> Ctx
addType (Ctx ctx) (n, ty) = Ctx ((n, ty) : ctx)

defaultCtx :: Ctx
defaultCtx =
  Ctx [("int_print", Arr [int] void), ("int_add", Arr [int, int] int)]

infer :: Module -> InferResult
infer mod = do
  (_, ty) <-
    foldM (\(ctx, _) stmt -> i_stmt ctx stmt) (defaultCtx, void) (stmts mod)
  return ty

i_stmt :: Ctx -> Stmt -> InferResultT (Ctx, Type)
i_stmt ctx (Expr expr) = do
  ty <- i_expr ctx expr
  return (ctx, ty)
i_stmt ctx (FnStmt fn) = do
  ty <- i_fn ctx fn
  return (addType ctx (name fn, ty), ty)

i_fn :: Ctx -> Function -> InferResult
i_fn ctx fn =
  let tyArgs = map (\(TypedName _ ty) -> ty) (params fn)
  in return $ Arr tyArgs (retType fn)

i_expr :: Ctx -> Expr -> InferResult
i_expr _ (Literal lit) = i_lit lit
i_expr ctx (Ident (Global i)) =
  case getType i ctx of
    Nothing -> Left $ UnknownVariable i
    Just t -> return t
i_expr ctx (App fn args) = do
  tyFn <- i_expr ctx fn
  case tyFn of
    Arr tyArgs tyRet -> i_app ctx args tyArgs tyRet
    _ -> Left ArityMismatch

i_app :: Ctx -> [Expr] -> [Type] -> Type -> InferResult
i_app _ [] [] tyRet = return tyRet
i_app _ [] tyArgs tyRet = return $ Arr tyArgs tyRet
i_app ctx args [] tyRet =
  case tyRet of
    Arr tyArgs tyRet' -> i_app ctx args tyArgs tyRet'
    _ -> Left ArityMismatch
i_app ctx (arg:args) (tyArg:tyArgs) tyRet = do
  c_expr ctx arg tyArg
  i_app ctx args tyArgs tyRet

c_expr :: Ctx -> Expr -> Type -> CheckResult
c_expr ctx expr ty = do
  tyExpr <- i_expr ctx expr
  when (tyExpr /= ty) (Left $ TypeError ty tyExpr)

i_lit :: Literal -> InferResult
i_lit (Integer _) = return int
i_lit (Float _) = return float
i_lit (Char _) = return char
i_lit (String _) = return string
