module Renamer.Env
  ( Env(getBinds)
  , RnName(..)
  , defaultEnv
  , addInternal
  , addLocal
  , lookupIdent
  , lookupRdrName
  , extendEnv
  , joinName
  ) where

import Renamer.State
import Renamer.Error

import Data.List (intercalate)

type RdrName = (Maybe String, String)

data RnName
  = External (String, String)
  | Internal String

data Env = Env { getBinds :: [(RdrName, RnName)] }

defaultEnv :: Env
defaultEnv = Env { getBinds = mkBuiltins builtins }

mkBuiltins :: [String] -> [(RdrName, RnName)]
mkBuiltins =
  map $ \n ->
    ((Nothing, n), Internal n)

builtins :: [String]
builtins =
  -- TYPES
  [ "Int"
  , "Float"
  , "Char"
  , "String"
  , "Void"
  , "Bool"

  -- TYPE CONSTRUCTORS
  , "List"

  -- DATA CONSTRUCTORS
  , "True"
  , "False"
  , "Nil"
  , "Cons"

  -- FUNCTIONS
  , "int_add"
  , "int_sub"
  , "int_mul"
  , "int_div"
  , "int_neg"
  , "int_to_string"
  , "string_print"
  ]


addLocal :: Env -> String -> Rn (Env, String)
addLocal env name = do
  mod <- thisMod
  let getBinds' = ((Nothing, name), External (mod, name)) : getBinds env
  let env' = Env { getBinds = getBinds' }
  return (env', joinName (mod, name))


addInternal :: Env -> String -> Rn Env
addInternal env name = do
  let getBinds' = ((Nothing, name), Internal name) : getBinds env
  let env' = Env { getBinds = getBinds' }
  return env'


lookupIdent :: [String] -> Env -> Rn String
lookupIdent [] _ = undefined
lookupIdent [x] env = lookupRdrName (Nothing, x) env
lookupIdent xs env =
  let mod = intercalate "." $ init xs
      name = last xs
   in lookupRdrName (Just mod, name) env


lookupRdrName :: RdrName -> Env -> Rn String
lookupRdrName rdrName env =
  case lookup rdrName (getBinds env) of
    Nothing -> throwError $ UnknownVariable $ showRdrName rdrName
    Just (Internal n) -> return n
    Just (External n) -> return (joinName n)


showRdrName :: RdrName -> String
showRdrName (Nothing, name) = name
showRdrName (Just mod, name) = joinName (mod, name)


joinName :: (String, String) -> String
joinName (mod, n) = mod ++ "." ++ n

extendEnv :: Env -> [(RdrName, RnName)] -> Env
extendEnv env binds =
  Env { getBinds = binds ++ getBinds env }
