module Interpreter.Env
  ( Env
  , defaultEnv
  , addVal
  , lookupVal
  , addCont
  , lookupCont
  , importEvalEnv
  ) where

import Core.Absyn (ContVar(..), Var(..))
import Interpreter.Value (ContValue(..), Value(..))
import Lib.Registry
import qualified Util.PrettyPrint as PP

data Env
  = Empty
  | ValueBind Var Value Env
  | ContBind ContVar ContValue Env

instance Show Env where
  show Empty = "{}"
  show (ValueBind x _ rest) =
    "{ " ++ PP.print x ++ showTail rest
  show (ContBind k _ rest) =
    "{ " ++ PP.print k ++ showTail rest

showTail :: Env -> String
showTail Empty = " }"
showTail (ValueBind x _ rest) =
  ", " ++ PP.print x ++ showTail rest
showTail (ContBind k _ rest) =
  ", " ++ PP.print k ++ showTail rest

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
lookupCont k (ContBind l v tail)
  | k == l = v
  | otherwise =  lookupCont k tail

addVal :: Env -> (Var, Value) -> Env
addVal env (x, v) = ValueBind x v env

addCont :: Env -> (ContVar, ContValue) -> Env
addCont env (k, v) = ContBind k v env

importEvalEnv :: [String] -> Env -> Env -> Env
importEvalEnv _ targetEnv Empty =
  targetEnv

importEvalEnv imports targetEnv (ContBind _ _ tail) =
  importEvalEnv imports targetEnv tail

importEvalEnv imports targetEnv (ValueBind x@(Var name) v tail)
  | name `elem` imports
  || head name == '%' =
    ValueBind x v (importEvalEnv imports targetEnv tail)
  | otherwise =
    importEvalEnv imports targetEnv tail
