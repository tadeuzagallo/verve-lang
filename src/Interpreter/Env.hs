module Interpreter.Env
  ( Env
  , defaultEnv
  , addVal
  , lookupVal
  , addCont
  , lookupCont
  ) where

import Core.Absyn (ContVar(..), Var(..))
import Interpreter.Value (ContValue(..), Value(..))
import Lib.Registry

data Env
  = Empty
  | ValueBind Var Value Env
  | ContBind ContVar ContValue Env
  deriving (Show)

defaultEnv :: Env
defaultEnv =
  let halt = ContVar "halt"
      env = ContBind halt Halt Empty
      filtered =  filter (\x -> isValue x || isInternal x) registry
      merge env entry =
        let (var, value) = impl entry
         in ValueBind (Var var) value env
   in foldl merge env filtered

lookupVal :: Var -> Env -> Value
lookupVal (Var x) Empty = VIn x []
lookupVal x (ContBind _ _ tail) = lookupVal x tail
lookupVal x (ValueBind y v tail)
  | x == y = v
  | otherwise = lookupVal x tail

lookupCont :: ContVar -> Env -> ContValue
lookupCont _ Empty = undefined
lookupCont k (ValueBind _ _ tail) = lookupCont k tail
lookupCont l (ContBind k v tail)
  | k == l = v
  | otherwise = lookupCont k tail

addVal :: Env -> (Var, Value) -> Env
addVal env (x, v) = ValueBind x v env

addCont :: Env -> (ContVar, ContValue) -> Env
addCont env (k, v) = ContBind k v env
