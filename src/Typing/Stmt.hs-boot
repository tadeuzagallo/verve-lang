module Typing.Stmt where

import Typing.Env
import Typing.Types

import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

i_stmts :: [U.Stmt] -> Tc ([T.Stmt], Maybe Type)
