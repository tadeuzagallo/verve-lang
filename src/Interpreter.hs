module Interpreter
  ( eval
  , evalWithEnv
  , Env
  , defaultEnv
  ) where

import CoreAbsyn
import Absyn (Literal(..), Name)
import Error
import Types (Type)

import Control.Monad (foldM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

type EvalResultT = Either Error
type EvalResult = EvalResultT Value

data Env =
  Env [(String, Value)]
  deriving (Show)

data RuntimeError
  = Unsupported
  | UnknownVariable String
  | MatchFailure
  deriving (Show)

instance ErrorT RuntimeError where
  kind _ = "RuntimeError"

data Value
  = VLit Literal
  | VLam (Value -> EvalResult)
  | VVoid
  | VType Type
  | VNeutral Neutral
  | VRecord [(String, Value)]

instance Show Value where
  show (VLit v) = show v
  show (VNeutral n) = show n
  show (VLam _) = "<function>"
  show VVoid = "()"
  show (VRecord fields) =
    "{" ++ fields' ++ "}"
      where
        fields' = intercalate ", " $ map showField fields
        showField (key, value) = key ++ " = " ++ show value

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
  , ("#fieldAccess"
    , VLam (\(VLit (String field)) ->
        return . VLam $ \(VRecord fields) ->
          return . fromJust $ lookup field fields))
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
evalWithEnv env exp =
  (,) env <$> e_expr env exp

e_expr :: Env -> Expr -> EvalResult
e_expr env Void = return VVoid
e_expr env (Lit s) = return $ VLit s
e_expr env (Var (name, _)) =
  case getValue name env of
    Nothing -> return . VNeutral $ NFree name
    Just val -> return val
e_expr env (Lam (name, _) body) = do
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

e_expr env (Match expr cases) = do
  v <- e_expr env expr
  e_cases env v cases

e_expr env (Record fields) = do
  fieldValues <- mapM (e_expr env . snd) fields
  let labels = map (fst . fst) fields
  return . VRecord $ zip labels fieldValues

e_cases :: Env -> Value -> [Case] -> EvalResult
e_cases _ _ [] = mkError MatchFailure
e_cases env val ((pattern, expr):cases) =
  case e_pattern env val pattern of
    Nothing -> e_cases env val cases
    Just env' -> e_expr env' expr

e_pattern :: Env -> Value -> Pattern -> Maybe Env
e_pattern env _ PatDefault = Just env
e_pattern env val (PatVar (name, _)) =
  Just (addValue env (name, val))
e_pattern env (VLit l) (PatLiteral l') =
  if l == l'
     then Just env
     else Nothing
e_pattern env (VNeutral n) (PatCtor (name, _) pats) =
  aux env n pats
    where
      aux env (NFree n') [] | n' == name = Just env
      aux env (NApp n' v) (p:ps) =
        e_pattern env v p >>= \env' -> aux env' n' ps
      aux _ _ _ = Nothing

e_let :: Env -> [Bind] -> Expr -> EvalResultT (Env, Value)
e_let env binds exp = do
  env' <- foldM e_bind env binds
  val <- e_expr env' exp
  return (env', val)

e_bind :: Env -> Bind -> EvalResultT Env
e_bind env ((name, _), exp) = do
  exp' <- e_expr env exp
  return $ addValue env (name, exp')
