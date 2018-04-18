{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Interpreter.Eval
  ( eval
  , evalWithEnv
  ) where

import Core.Absyn
import Interpreter.Env
import qualified Interpreter.Value as Rt

import Data.List (find)

eval :: Term -> Rt.Value
eval = snd . evalTerm defaultEnv

evalWithEnv, evalTerm :: Env -> Term -> (Env, Rt.Value)
evalWithEnv = evalTerm

evalTerm _ Error =
  error "pattern match failed"

evalTerm env (LetVal x v k) =
  let v' = evalValue env v
      env' = addVal env (x, v')
   in evalTerm env' k

evalTerm env (LetCont defs l) =
  let env' = foldl (\env def -> addCont env (evalContDef env' def)) env defs
   in evalTerm env' l

evalTerm env (LetFun defs t) =
  let env' = foldl (\env def -> addVal env (evalFunDef env' def)) env defs
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
       in
      case compare (length xs) (length ys) of
        -- partial application
        LT ->
          let ys' = drop (length vs) ys
           in evalCont env k [Rt.VClosure (env''', Lambda j ys' t)]

        -- curry - is this a good idea?
        GT ->
          let j = case k of ContVar k' -> ContVar (k' ++ "'")
              xs' = drop (length ys) xs
              def = ContDef j [f] (App f k $ xs')
           in evalTerm env (LetCont [def] $
             App f j (take (length ys) xs))

        -- nice
        EQ ->
           evalTerm env''' t
    Rt.VBuiltin _ ->
      -- force strictness since this is the only place where side effects can happen
      let !val = foldl g v xs
       in evalCont env k [val]
        where
          g (Rt.VBuiltin f) x =
            f (lookupVal x env)
    -- TODO: this hack with Halt is a bit sad
    Rt.VIn "#fix" [] ->
      let [Rt.VClosure (_, Lambda j [x] t)] = (`lookupVal` env) <$> xs
          env' = addVal (addCont env (j, Rt.Halt)) (x, v')
          (_, v') = evalTerm env' t
       in evalCont env k [v']
    Rt.VIn i vs ->
      let vs' = filter noTypes $ map (`lookupVal` env) xs
          -- discard types from sums
          noTypes (Rt.VType _) = False
          noTypes _ = True
       in evalCont env k [Rt.VIn i (vs ++ vs')]

evalTerm env (Case x ks) =
  let Rt.VIn i vs = lookupVal x env
      Just (Clause _ ki) = find (\(Clause c _) -> c == i) ks
  in evalCont env ki vs

evalCont :: Env -> ContVar -> [Rt.Value] -> (Env, Rt.Value)
evalCont env k vals =
  case lookupCont k env of
    Rt.Halt ->
      let f [] = Rt.VUnit
          f (x : _) = x
       in (env, f vals)
    Rt.Cont (env', (xs, t)) ->
      let env'' = foldl addVal env' (zip xs vals)
       in evalTerm env'' t

evalValue :: Env -> Value -> Rt.Value
evalValue _ Unit =
  Rt.VUnit

evalValue env (Lam f) =
  Rt.VClosure (env, f)

evalValue _ (Lit l) =
  Rt.VLit l

evalValue env (In i args) =
  Rt.VIn i (map (flip lookupVal env) args)

evalValue env (Record fields) =
  Rt.VRecord (map f fields)
    where f (name, var) =
            (name, lookupVal var env)

evalValue _ (Type t) =
  Rt.VType t

evalContDef :: Env -> ContDef -> (ContVar, Rt.ContValue)
evalContDef env (ContDef contVar vars body) =
  let cont = Rt.Cont (env, (vars, body))
   in (contVar, cont)

evalFunDef :: Env -> FunDef -> (Var, Rt.Value)
evalFunDef env (FunDef name k xs body) =
  let val = Rt.VClosure (env, (Lambda k xs body))
   in (name, val)
