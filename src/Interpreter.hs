module Interpreter
  ( eval
  , evalWithEnv
  , Env
  , defaultEnv
  ) where

import CoreAbsyn
import Absyn (Literal(..), Id(..), Name)
import Error
import Types (Type)

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
  | VType Type
  | VNeutral Neutral

instance Show Value where
  show (VLit v) = show v
  show (VNeutral n) = show n
  show (VLam _) = "<function>"
  show VVoid = "()"

data Neutral
  = NFree Name
  | NApp Neutral Value

instance Show Neutral where
  show (NFree n) = n
  show (NApp n v) = show n ++ " " ++ show v

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
evalWithEnv env (Let binds exp) =
  e_let env binds exp

e_expr :: Env -> Expr -> EvalResult
e_expr env Void = return VVoid
e_expr env (Lit s) = return $ VLit s
e_expr env (Var (Id name _)) =
  case getValue name env of
    Nothing -> return . VNeutral $ NFree name
    Just val -> return val
e_expr env (Lam (Id name _) body) = do
  return . VLam $ \v -> e_expr (addValue env (name, v)) body
e_expr _ (Type t) = return $ VType t
e_expr env (App fn arg) = do
  fnVal <- e_expr env fn
  arg' <- e_expr env arg
  case fnVal of
    VNeutral n -> return . VNeutral $ NApp n arg'
    VLam f -> f arg'

e_expr env (Let binds exp) =
  e_let env binds exp >>= return . snd

e_let :: Env -> [Bind] -> Expr -> EvalResultT (Env, Value)
e_let env binds exp = do
  env' <- foldM e_bind env binds
  val <- e_expr env' exp
  return (env', val)

e_bind :: Env -> Bind -> EvalResultT Env
e_bind env (Id name _, exp) = do
  exp' <- e_expr env exp
  return $ addValue env (name, exp')
