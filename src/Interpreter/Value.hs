module Interpreter.Value
  ( Value(..)
  , ContValue(..)
  ) where

import Absyn.Meta (Literal)
import Core.Absyn (Lambda, Var, Term)
import {-# SOURCE #-} Interpreter.Env (Env)
import Typing.Types (Type)

import Data.List (intercalate)

data Value
  = VLit Literal
  | VClosure (Env, Lambda)
  | VBuiltin (Value -> Value)
  | VUnit
  | VType Type
  | VIn String [Value]
  | VRecord [(String, Value)]

data ContValue
  = Cont (Env, ([Var], Term))
  | Halt
  deriving (Show)

-- Show
instance Show Value where
  show (VLit v) = show v
  show (VClosure _) = "<closure>"
  show (VBuiltin _) = "<builtin>"
  show VUnit = "()"
  show (VType v) = show v
  show (VRecord v) = show v
  show (VIn c vs) =
    c ++ f vs
      where
        f [] = ""
        f args = "(" ++ intercalate ", " (map show args) ++ ")"
