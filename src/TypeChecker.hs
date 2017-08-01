module TypeChecker
  ( infer
  , inferStmt
  , Ctx
  , defaultCtx
  , TypeError
  ) where

import Absyn
import Error
import Types

import Control.Monad (foldM, when)
import Text.Printf (printf)

data TypeError
  = UnknownVariable String
  | ArityMismatch
  | TypeError { expected :: Type
              , actual :: Type }
  deriving (Show)

instance ErrorT TypeError where
  kind _ = "TypeError"

type Result = Either Error

type InferResult = Result Type

type CheckResult = Result ()

data Ctx =
  Ctx [(String, Type)]
      [Type]

getGlobal :: String -> Ctx -> Maybe Type
getGlobal n (Ctx globals _) = lookup n globals

addGlobal :: Ctx -> (String, Type) -> Ctx
addGlobal (Ctx globals locals) (n, ty) = Ctx ((n, ty) : globals) locals

getLocal :: Int -> Ctx -> Type
getLocal i (Ctx _ locals) = locals !! i

addLocals :: Ctx -> [Type] -> Ctx
addLocals (Ctx globals locals) types = Ctx globals (reverse types ++ locals)

defaultCtx :: Ctx
defaultCtx =
  Ctx [("int_print", Arr [int] void), ("int_add", Arr [int, int] int)] []

infer :: Module -> InferResult
infer mod = i_stmts defaultCtx (stmts mod)

inferStmt :: Ctx -> Stmt -> Result (Ctx, Type)
inferStmt = i_stmt

i_stmts :: Ctx -> [Stmt] -> InferResult
i_stmts ctx stmts = do
  (_, ty) <- foldM (\(ctx, _) stmt -> i_stmt ctx stmt) (ctx, void) stmts
  return ty

c_stmts :: Ctx -> [Stmt] -> Type -> CheckResult
c_stmts ctx stmts ty = do
  actualTy <- i_stmts ctx stmts
  when (actualTy /= ty) (mkError $ TypeError ty actualTy)

i_stmt :: Ctx -> Stmt -> Result (Ctx, Type)
i_stmt ctx (Expr expr) = do
  ty <- i_expr ctx expr
  return (ctx, ty)
i_stmt ctx (FnStmt fn) = do
  ty <- i_fn ctx fn
  return (addGlobal ctx (name fn, ty), ty)

i_fn :: Ctx -> Function -> InferResult
i_fn ctx fn = do
  let tyArgs =
        case (params fn) of
          [] -> [void]
          p -> map (\(TypedName _ ty) -> ty) p
  let ty = Arr tyArgs (retType fn)
  c_stmts (addLocals ctx tyArgs) (body fn) (retType fn)
  return ty

i_expr :: Ctx -> Expr -> InferResult
i_expr _ (Literal lit) = i_lit lit
i_expr ctx (Ident (Global i)) =
  case getGlobal i ctx of
    Nothing -> mkError $ UnknownVariable i
    Just t -> return t
i_expr ctx (Ident (Local i)) = return $ getLocal i ctx
i_expr ctx VoidExpr = return void
i_expr ctx (App fn []) = i_expr ctx (App fn [VoidExpr])
i_expr ctx (App fn args) = do
  tyFn <- i_expr ctx fn
  i_app ctx args [] tyFn

i_app :: Ctx -> [Expr] -> [Type] -> Type -> InferResult
i_app _ [] [] tyRet = return tyRet
i_app _ [] tyArgs tyRet = return $ Arr tyArgs tyRet
i_app ctx args [] tyRet =
  case tyRet of
    Arr tyArgs tyRet' -> i_app ctx args tyArgs tyRet'
    _ -> mkError ArityMismatch
i_app ctx (arg:args) (tyArg:tyArgs) tyRet = do
  c_expr ctx arg tyArg
  i_app ctx args tyArgs tyRet

c_expr :: Ctx -> Expr -> Type -> CheckResult
c_expr ctx expr ty = do
  tyExpr <- i_expr ctx expr
  when (tyExpr /= ty) (mkError $ TypeError ty tyExpr)

i_lit :: Literal -> InferResult
i_lit (Integer _) = return int
i_lit (Float _) = return float
i_lit (Char _) = return char
i_lit (String _) = return string
