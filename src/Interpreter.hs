module Interpreter
  ( eval
  , evalStmt
  , Env
  , defaultEnv
  , RuntimeError
  ) where

import Absyn
import Error

import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

type EvalResultT = Either Error

type EvalResult = EvalResultT Value

data Env = Env
  { globals :: [(String, Value)]
  , locals :: [Value]
  } deriving (Show)

data RuntimeError
  = Unsupported
  | UnknownVariable String
  deriving (Show)

instance ErrorT RuntimeError where
  kind _ = "RuntimeError"

data Value
  = VLit Literal
  | VLam (Value -> EvalResult)
  | Void

instance Show Value where
  show (VLit v) = show v
  show (VLam _) = "<function>"
  show Void = "()"

builtins :: [(String, Value)]
builtins =
  [ ( "int_add"
    , VLam
        (\(VLit (Integer a)) ->
           return . VLam $ \(VLit (Integer b)) ->
             return . VLit . Integer $ a + b))
  , ( "int_print"
    , VLam
        (\v ->
           case unsafePerformIO (print v) of
             () -> return Void))
  ]

defaultEnv :: Env
defaultEnv = Env {globals = builtins, locals = []}

addLocals :: Env -> [Value] -> Env
addLocals env locals' = env {locals = reverse locals' ++ (locals env)}

addGlobal :: Env -> (String, Value) -> Env
addGlobal env (n, val) = env {globals = (n, val) : (globals env)}

eval :: Module -> EvalResult
eval mod = e_stmts defaultEnv (stmts mod)

evalStmt :: Env -> Stmt -> EvalResultT (Env, Value)
evalStmt = e_stmt

e_stmts :: Env -> [Stmt] -> EvalResult
e_stmts env stmts = do
  (_, val) <- foldM (\(env, _) stmt -> e_stmt env stmt) (env, Void) stmts
  return val

e_stmt :: Env -> Stmt -> EvalResultT (Env, Value)
e_stmt env (Expr expr) = do
  val <- e_expr env expr
  return (env, val)
e_stmt env (FnStmt fn) = do
  val <- e_fn env fn
  return (addGlobal env (name fn, val), val)

e_fn :: Env -> Function -> EvalResult
e_fn env fn = do
  wrap env (params fn)
  where
    wrap :: Env -> [TypedName] -> EvalResult
    wrap env [] = body' env
    wrap env (param:params) =
      return . VLam $ \v -> wrap (addLocals env [v]) params
    body' :: Env -> EvalResult
    body' env =
      let b = e_stmts env (body fn)
      in case params fn of
           [] -> return . VLam $ \_ -> b
           _ -> b

e_expr :: Env -> Expr -> EvalResult
e_expr _ (Literal s) = return $ VLit s
e_expr env (Ident (Local id)) = return $ locals env !! id
e_expr env (Ident (Global id)) =
  case lookup id (globals env) of
    Nothing -> mkError $ UnknownVariable id
    Just val -> return val
e_expr env VoidExpr = return Void
e_expr env (App fn []) = e_expr env (App fn [VoidExpr])
e_expr env (App fn args) = do
  fn' <- e_expr env fn
  foldM app fn' args
  where
    app :: Value -> Expr -> EvalResult
    app (VLam fn) arg = e_expr env arg >>= fn
e_expr _ _ = mkError Unsupported
