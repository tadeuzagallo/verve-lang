module Interpreter.Value
  ( Value(..)
  , Neutral(..)
  , EvalResult
  , EvalResultT
  ) where

import Absyn.Meta (Literal, Name)
import Typing.Types (Type)
import Util.Error
import Util.PrettyPrint (PrettyPrint(ppr), pprName)

import Data.List (intercalate)

type EvalResultT = Either Error
type EvalResult = EvalResultT Value

data Value
  = VLit Literal
  | VLam (Value -> EvalResult)
  | VVoid
  | VType Type
  | VNeutral Neutral
  | VRecord [(String, Value)]
  | VLazy (() -> EvalResult)

data Neutral
  = NFree Name
  | NApp Neutral Value

instance Show Value where
  show (VType t) = show t
  show (VNeutral n) = show n
  show v = showValue show v

instance PrettyPrint Value where
  ppr (VType t) = ppr t
  ppr (VNeutral n) = ppr n
  ppr v = showValue ppr v

showValue :: (Value -> String) -> Value -> String
showValue _ (VType _) = undefined
showValue _ (VNeutral _) = undefined
showValue _  VVoid = "()"
showValue _ (VLam _) = "<function>"
showValue _ (VLazy _) = "<lazy>"
showValue _ (VLit v) = show v
showValue pv (VRecord fields) =
  "{" ++ fields' ++ "}"
    where
      fields' = intercalate ", " $ map showField fields
      showField (key, value) = key ++ ": " ++ pv value

instance Show Neutral where
  show (NFree n) = n
  show app = showNeutral show show app

instance PrettyPrint Neutral where
  ppr (NFree n) = pprName n
  ppr app = showNeutral ppr ppr app

showNeutral :: (Neutral -> String) -> (Value -> String) -> Neutral -> String
showNeutral _ _ (NFree _) = undefined
showNeutral pn pv (NApp n v) = pn n ++ "(" ++ pv v ++ ")"
