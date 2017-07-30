module Interpreter
  ( eval
  ) where

import Absyn

import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)

type EvalResultT = Either RuntimeError

type EvalResult = EvalResultT Value

data Env = Env
  { globals :: [(String, Value)]
  , locals :: [Value]
  } deriving (Show)

data RuntimeError
  = Unsupported
  | UnknownVariable String
  deriving (Show)

data Value
  = VLit Literal
  | VLam (Env -> EvalResult)
  | Void

instance Show Value where
  show (VLit v) = show v
  show (VLam _) = "<function>"
  show Void = "()"

builtins :: [(String, Value)]
builtins =
  [ ( "int_add"
    , VLam
        (\(Env {locals = (VLit (Integer a)):(VLit (Integer b)):_}) ->
           return . VLit . Integer $ a + b))
  , ( "int_print"
    , VLam
        (\(Env {locals = v:_}) ->
           case unsafePerformIO (print v) of
             () -> return Void))
  ]

defaultEnv :: Env
defaultEnv = Env {globals = builtins, locals = []}

addLocals :: Env -> [Value] -> Env
addLocals env locals' = env {locals = locals' ++ (locals env)}

addGlobal :: Env -> (String, Value) -> Env
addGlobal env (n, val) = env {globals = (n, val) : (globals env)}

eval :: Module -> EvalResult
eval mod = e_stmts defaultEnv (stmts mod)

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
e_fn _ fn = do
  return . VLam $ \env -> e_stmts env (body fn)

e_expr :: Env -> Expr -> EvalResult
e_expr _ (Literal s) = return $ VLit s
e_expr env (Ident (Local id)) = return $ locals env !! id
e_expr env (Ident (Global id)) =
  case lookup id (globals env) of
    Nothing -> Left $ UnknownVariable id
    Just val -> return val
e_expr env (App fn args) = do
  args' <- mapM (e_expr env) args
  VLam f <- e_expr env fn
  let env' = addLocals env args'
  f env'
e_expr _ _ = Left Unsupported
