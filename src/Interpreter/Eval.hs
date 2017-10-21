{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Interpreter.Eval
  ( eval
  , evalWithEnv
  ) where

import Core.Absyn
import Interpreter.Env
import Interpreter.Value
import Interpreter.RuntimeError

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Function (fix)


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
e_expr _ Void = return VVoid
e_expr _ (Lit s) = return $ VLit s
e_expr env (Var (name, _)) =
  case getValue name env of
    Nothing -> return . VNeutral $ NFree name
    Just (VLazy f) -> f ()
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

e_pattern env (VRecord vFields) (PatRecord patFields) =
  foldM aux env vFields
  where
    vFields' = map (first fst) patFields

    aux env (key, val) = do
      pat <- lookup key vFields'
      e_pattern env val pat

e_pattern env (VNeutral n) (PatCtor (name, _) pats) =
  aux env n pats
    where
      aux env (NFree n') [] | n' == name = Just env
      aux env (NApp n (VType _)) p =
        aux env n p
      aux env (NApp n' v) (p:ps) =
        e_pattern env v p >>= \env' -> aux env' n' ps
      aux _ _ _ = Nothing

e_let :: Env -> [Bind] -> Expr -> EvalResultT (Env, Value)
e_let env binds exp = do
  let names = map (fst . fst) binds
  let mkFn (_, expr) values = VLazy $ \() ->
        let env' = foldl addValue env (zip names values)
         in e_expr env' expr
  let fns = map mkFn binds
  let env' = foldl addValue env $ zip names (mfix_poly fns)
  val <- e_expr env' exp
  return (env', val)
 where
   mfix_poly :: [[a] -> a] -> [a]
   mfix_poly fns = fix (\self -> map ($ self) fns)
