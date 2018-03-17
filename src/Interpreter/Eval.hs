{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Interpreter.Eval
  ( eval
  , evalWithEnv
  ) where

import Core.Absyn
import Interpreter.Env
import qualified Interpreter.Value as Rt

import Debug.Trace

eval :: Term -> Rt.Value
eval = snd . evalTerm defaultEnv

evalWithEnv, evalTerm :: Env -> Term -> (Env, Rt.Value)
evalWithEnv = evalTerm

evalTerm env (LetVal x v k) =
  let v' = evalValue env v
      env' = addVal env (x, v')
   in evalTerm env' k

evalTerm env (LetCont defs l) =
  let conts = map (evalContDef env) defs
      env' = foldl addCont env conts
   in evalTerm env' l

evalTerm env (LetFun defs t) =
  let funs = map (evalFunDef env) defs
      env' = foldl addVal env funs
   in evalTerm env' t

evalTerm env (AppCont k xs) =
  evalCont env k (map (flip lookupVal env) xs)

evalTerm env (App f k xs) =
  let v = lookupVal f env in
    case v of
    Rt.VClosure (env', Lambda j ys t) ->
      let env'' = addCont env' (j, lookupCont k env)
          vs = map (`lookupVal` env) xs
          env''' = foldl addVal env'' (zip ys vs)
       in evalTerm env''' t
    Rt.VBuiltin _ ->
      let val = foldl g v xs
       in evalCont env k [val]
        where g (Rt.VBuiltin f) x =
                (trace . show) (lookupVal x env) $
                  f (lookupVal x env)

evalTerm env (Match x ks) =
  error "match not supported yet"
  {-let Rt.VIn i vs = lookupVal x env-}
      {-ki = ks !! i-}
      {-Rt.Cont (env', (ys, t)) = lookupCont ki env-}
      {-env'' = foldl addVal env' (zip ys vs)-}
   {-in evalTerm env'' t-}

evalCont :: Env -> ContVar -> [Rt.Value] -> (Env, Rt.Value)
evalCont env k vals =
  case lookupCont k env of
    Rt.Halt ->
      (env, head vals)
    Rt.Cont (env', (xs, t)) ->
      let env'' = foldl addVal env' (zip xs vals)
       in evalTerm env'' t

evalValue :: Env -> Value -> Rt.Value
evalValue _ Unit =
  Rt.VUnit

evalValue env (Lam f) =
  Rt.VClosure (env, f)

evalValue env (Lit l) =
  Rt.VLit l

evalValue env (In i args) =
  Rt.VIn i (map (flip lookupVal env) args)

evalValue env (Record fields) =
  Rt.VRecord (map f fields)
    where f ((name, _), var) =
            (name, lookupVal var env)

evalValue env (Type t) =
  Rt.VType t

evalContDef :: Env -> ContDef -> (ContVar, Rt.ContValue)
evalContDef env (ContDef contVar vars body) =
  let cont = Rt.Cont (env, (vars, body))
   in (contVar, cont)

-- TODO: this is wrong and won't support mutual recursive fns
evalFunDef :: Env -> FunDef -> (Var, Rt.Value)
evalFunDef env (FunDef name k xs body) =
  let val = Rt.VClosure (env, (Lambda k xs body))
   in (name, val)
