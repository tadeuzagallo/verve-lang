module Interpreter
  ( eval
  , evalWithEnv
  , Env
  , defaultEnv
  ) where

import CoreAbsyn
import Absyn (Literal(..), Id(..))
import Error

import Control.Monad (foldM)
import System.IO.Unsafe (unsafePerformIO)

type EvalResultT = Either Error
type EvalResult = EvalResultT Value

data Env =
  Env [(String, Value)]
  deriving (Show)

data RuntimeError
  = Unsupported
  | UnknownVariable String
  deriving (Show)

instance ErrorT RuntimeError where
  kind _ = "RuntimeError"

data Value
  = VLit Literal
  | VLam (Value -> EvalResult)
  | VVoid

instance Show Value where
  show (VLit v) = show v
  show (VLam _) = "<function>"
  show VVoid = "()"

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
             () -> return VVoid))
  ]

defaultEnv :: Env
defaultEnv = Env builtins

addValue :: Env -> (String, Value) -> Env
addValue (Env env) (n, val) = Env ((n, val) : env)

getValue :: String -> Env -> Maybe Value
getValue key (Env env) = lookup key env

eval :: Expr -> EvalResult
eval expr = do
  (_, val) <- evalWithEnv defaultEnv expr
  return val

evalWithEnv :: Env -> Expr -> EvalResultT (Env, Value)
evalWithEnv env Void = return (env, VVoid)
evalWithEnv env (Lit s) = return $ (env, VLit s)
evalWithEnv env (Var (Id name _)) =
  case getValue name env of
    Nothing -> mkError $ UnknownVariable name
    Just val -> return (env, val)
evalWithEnv env (App fn arg) = do
  (_, VLam fn') <- evalWithEnv env fn
  (_, arg') <- evalWithEnv env arg
  val <- fn' arg'
  return (env, val)
evalWithEnv env (Lam (Id name _) body) = do
  return $ (,) env $ VLam $ \v -> evalWithEnv (addValue env (name, v)) body >>= return . snd
evalWithEnv env (Let binds exp) = do
  env' <- e_binds env binds
  (_, val) <- evalWithEnv env' exp
  return (env', val)

e_binds :: Env -> [Bind] -> EvalResultT Env
e_binds env [] = return env
e_binds env ((Id name _, exp):bs) = do
  (_, exp') <- evalWithEnv env exp
  e_binds (addValue env (name, exp')) bs
