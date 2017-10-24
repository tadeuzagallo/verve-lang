module Interpreter.Env
  ( Env
  , defaultEnv
  , addValue
  , getValue
  ) where

import Interpreter.Builtin
import Interpreter.Value (Value)

data Env =
  Env [(String, Value)]
  deriving (Show)

defaultEnv :: Env
defaultEnv = Env [ ("int_add", int_add)
                 , ("int_sub", int_sub)
                 , ("int_mul", int_mul)
                 , ("int_div", int_div)
                 , ("int_neg", int_neg)
                 , ("int_to_string", int_to_string)
                 , ("string_print", string_print)

                 -- Private Values
                 , ("#fieldAccess", fieldAccess)
                 , ("#unwrapClass", unwrapClass)
                 ]

addValue :: Env -> (String, Value) -> Env
addValue (Env env) (n, val) = Env ((n, val) : env)

getValue :: String -> Env -> Maybe Value
getValue key (Env env) = lookup key env
