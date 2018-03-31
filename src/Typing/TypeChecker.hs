{-# LANGUAGE NamedFieldPuns #-}

module Typing.TypeChecker
  ( inferStmts
  ) where

import Typing.Ctx
import Typing.State
import Typing.Stmt
import Typing.Types

import Util.Error
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

inferStmts :: Ctx -> [U.Stmt] -> Result (Ctx, [T.Stmt], Maybe Type)
inferStmts ctx stmts =
  runTc (i_stmts ctx stmts) id
