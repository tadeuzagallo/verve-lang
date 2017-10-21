module Reassoc.Env
  ( Env
  , PrecInt
  , defaultEnv
  , addOpInfo
  , getOpInfo
  , getPrec
  ) where

import Absyn.Untyped
import Reassoc.Error
import Util.Error

type PrecInt = Integer
type OpInfo = (Associativity, PrecInt)

newtype Env = Env [(Name, OpInfo)]

defaultEnv :: Env
defaultEnv = Env []

addOpInfo :: Env -> (Name, OpInfo) -> Env
addOpInfo (Env env) info = Env (info : env)

getOpInfo :: Name -> Env -> Result OpInfo
getOpInfo name (Env env) =
  case lookup name env of
    Just info -> return info
    Nothing -> mkError $ UnknownOperator name

getPrec :: Name -> Env -> Result PrecInt
getPrec name env = snd <$> getOpInfo name env
