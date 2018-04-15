module Typing.TypeChecker
  ( inferStmts

  , TcEnv
  , defaultEnv

  , importModule
  ) where

import Typing.Env
import Typing.Stmt
import Typing.Types

import Util.Error
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

inferStmts :: TcEnv -> [U.Stmt] -> Result (TcEnv, ([T.Stmt], Maybe Type))
inferStmts state stmts =
  runScoped (i_stmts stmts) state
