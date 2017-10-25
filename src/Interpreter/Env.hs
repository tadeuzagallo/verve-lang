module Interpreter.Env
  ( Env
  , defaultEnv
  , addValue
  , getValue
  ) where

import Interpreter.Value (Value)
import Lib.Registry

data Env =
  Env [(String, Value)]
  deriving (Show)

defaultEnv :: Env
defaultEnv = Env (impl <$> filter (\x -> isValue x || isInternal x) registry)

addValue :: Env -> (String, Value) -> Env
addValue (Env env) (n, val) = Env ((n, val) : env)

getValue :: String -> Env -> Maybe Value
getValue key (Env env) = lookup key env
