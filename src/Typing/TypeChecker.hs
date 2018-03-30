{-# LANGUAGE NamedFieldPuns #-}

module Typing.TypeChecker
  ( inferStmt
  , inferStmts
  ) where

import Typing.Ctx
import Typing.State
import Typing.Stmt
import Typing.Types

import Util.Error
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

inferStmts :: Ctx -> [U.Stmt] -> Result (Ctx, [T.Stmt], Type)
inferStmts ctx stmts =
  runTc (i_stmts ctx stmts) id

inferStmt :: Ctx -> U.Stmt -> Result (Ctx, T.Stmt, Type)
inferStmt ctx stmt =
  runTc (i_stmt ctx stmt) id
