module Typing.TypeChecker
  ( inferStmts
  ) where

import Typing.State
import Typing.Stmt
import Typing.Types

import Util.Error
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

inferStmts :: TcState -> [U.Stmt] -> Result (TcState, ([T.Stmt], Maybe Type))
inferStmts state stmts =
  runTc (i_stmts stmts) state
