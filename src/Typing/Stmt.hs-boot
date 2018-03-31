module Typing.Stmt where

import Typing.Ctx
import Typing.State
import Typing.Types

import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

i_stmts :: Ctx -> [U.Stmt] -> Tc (Ctx, [T.Stmt], Maybe Type)
