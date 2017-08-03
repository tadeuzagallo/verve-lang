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

data TypeError
  = UnknownVariable String
  | ArityMismatch
  | TypeError { expected :: Type
              , actual :: Type }
  deriving (Show)

instance ErrorT TypeError where
  kind _ = "TypeError"

typeCheck :: Type -> Type -> Result ()
typeCheck actualTy expectedTy =
  when (actualTy /= expectedTy) (mkError $ TypeError expectedTy actualTy)

data Ctx = Ctx [(String, Type)]

getType :: String -> Ctx -> Maybe Type
getType n (Ctx ctx) = lookup n ctx

addType :: Ctx -> (String, Type) -> Ctx
addType (Ctx ctx) (n, ty) = Ctx ((n, ty) : ctx)

defaultCtx :: Ctx
defaultCtx =
  Ctx [("int_print", Arr [int] void), ("int_add", Arr [int, int] int)]

infer :: Module Name -> Result (Module Id, Type)
infer mod = do
  (stmts, ty) <- i_stmts defaultCtx (stmts mod)
  return (Module stmts, ty)

inferStmt :: Ctx -> Stmt Name -> Result (Ctx, Stmt Id, Type)
inferStmt = i_stmt

i_stmts :: Ctx -> [Stmt Name] -> Result ([Stmt Id], Type)
i_stmts ctx stmts = do
  (_, stmts', ty) <- foldM aux (ctx, [], void) stmts
  return (reverse stmts', ty)
    where
      aux :: (Ctx, [Stmt Id], Type) -> Stmt Name -> Result (Ctx, [Stmt Id], Type)
      aux (ctx, stmts, _) stmt = do
        (ctx', stmt', ty) <- i_stmt ctx stmt
        return (ctx', stmt':stmts, ty)


i_stmt :: Ctx -> Stmt Name -> Result (Ctx, Stmt Id, Type)
i_stmt ctx (Expr expr) = do
  (expr', ty) <- i_expr ctx expr
  return (ctx, Expr expr', ty)
i_stmt ctx (FnStmt fn) = do
  (fn', ty) <- i_fn ctx fn
  return (addType ctx (name fn, ty), FnStmt fn', ty)

i_fn :: Ctx -> Function Name -> Result (Function Id, Type)
i_fn ctx fn = do
  let tyArgs = params fn
  let ty =
        Arr
          (if null tyArgs
             then [void]
             else map snd tyArgs)
          (retType fn)
  let ctx' = foldl addType ctx tyArgs
  (body', bodyTy) <- i_stmts ctx' (body fn)
  typeCheck bodyTy (retType fn)
  let fn' = fn { name = Id (name fn) ty, body = body' }
  return (fn', ty)

i_expr :: Ctx -> Expr Name -> Result (Expr Id, Type)
i_expr _ (Literal lit) = return (Literal lit, i_lit lit)
i_expr ctx (Ident i) =
  case getType i ctx of
    Nothing -> mkError $ UnknownVariable i
    Just ty -> return (Ident (Id i ty), ty)
i_expr ctx VoidExpr = return (VoidExpr, void)
i_expr ctx (App fn []) = i_expr ctx (App fn [VoidExpr])
i_expr ctx (App fn args) = do
  (fn', tyFn) <- i_expr ctx fn
  (args', tyArgs) <- mapM (i_expr ctx) args >>= return . unzip
  retType <- i_app ctx tyArgs [] tyFn
  return (App fn' args', retType)

i_app :: Ctx -> [Type] -> [Type] -> Type -> Result Type
i_app _ [] [] tyRet = return tyRet
i_app _ [] tyArgs tyRet = return $ Arr tyArgs tyRet
i_app ctx args [] tyRet =
  case tyRet of
    Arr tyArgs tyRet' -> i_app ctx args tyArgs tyRet'
    _ -> mkError ArityMismatch
i_app ctx (actualTy:args) (expectedTy:tyArgs) tyRet = do
  typeCheck actualTy expectedTy
  i_app ctx args tyArgs tyRet

i_lit :: Literal -> Type
i_lit (Integer _) = int
i_lit (Float _) = float
i_lit (Char _) = char
i_lit (String _) = string
