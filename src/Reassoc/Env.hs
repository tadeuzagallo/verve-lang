module Reassoc.Env
  ( Reassoc
  , ReassocEnv
  , PrecInt
  , defaultEnv
  , addOpInfo
  , getOpInfo
  , getPrec
  , importReassocEnv
  ) where

import Reassoc.Error

import Absyn.Untyped
import Util.Error
import qualified Util.Scope as S

type PrecInt = Integer
type OpInfo = (Associativity, PrecInt)

newtype Env = Env { getEnv :: [(String, OpInfo)] }
  deriving (Show)

type Reassoc a = S.Scoped Env a
type ReassocEnv = S.Scope Env

instance S.Env Env where
  type KeyType Env = String
  type ValueType Env = OpInfo
  type InterfaceType Env = ()

  deleteBetween from to current = Env $
    S.deleteBetweenField (getEnv from) (getEnv to) (getEnv current)

defaultEnv :: ReassocEnv
defaultEnv =
  S.createScope
    []
    []
    []
    (Env [])

addOpInfo :: String -> OpInfo -> Reassoc ()
addOpInfo op info =
  S.updateEnv $ \e -> Env { getEnv = (op, info) : getEnv e }

getOpInfo :: String -> Reassoc OpInfo
getOpInfo op = do
  info <- S.lookupEnv getEnv op
  case info of
    Just info -> return info
    Nothing -> throwError $ UnknownOperator op

getPrec :: String -> Reassoc PrecInt
getPrec name = snd <$> getOpInfo name

importReassocEnv :: [String] -> ReassocEnv -> ReassocEnv -> ReassocEnv
importReassocEnv imports targetScope importScope =
  S.importEnv importer targetScope importScope
    where
      importer (Env targetEnv) (Env importEnv) =
        Env $ filter ((`elem` imports) . fst) importEnv ++ targetEnv
