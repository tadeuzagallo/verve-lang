module Reassoc.Env
  ( Env
  , PrecInt
  , defaultEnv
  , addOpInfo
  , getOpInfo
  , getPrec
  , importReassocEnv
  ) where

import Absyn.Untyped
import Reassoc.Error
import Util.Error

type PrecInt = Integer
type OpInfo = (Associativity, PrecInt)

newtype Env = Env [(String, OpInfo)]

defaultEnv :: Env
defaultEnv = Env []

addOpInfo :: Env -> (String, OpInfo) -> Env
addOpInfo (Env env) info = Env (info : env)

getOpInfo :: String -> Env -> Result OpInfo
getOpInfo name (Env env) =
  case lookup name env of
    Just info -> return info
    Nothing -> mkError $ UnknownOperator name

getPrec :: String -> Env -> Result PrecInt
getPrec name env = snd <$> getOpInfo name env

importReassocEnv :: [String] -> Env -> Env -> Env
importReassocEnv imports (Env targetEnv) (Env importEnv) =
  Env (filter ((`elem` imports) . fst) importEnv ++ targetEnv)
