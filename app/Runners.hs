module Runners
  ( StmtsFn
  , Verbosity(..)
  , runEach
  , runAll
  , runStmt
  ) where

import Env
import Options
import TypedValue

import Absyn.Untyped (Stmt)
import Core.Desugar (desugarStmts)
import Interpreter.Eval (evalWithEnv)
import Reassoc.Reassoc (reassocStmts)
import Renamer.Renamer (renameStmts)
import Typing.TypeChecker (inferStmts)
import qualified Util.PrettyPrint as PP

import Control.Monad (when)

type StmtsFn = String -> [Stmt] -> Pipeline ()

data Verbosity
  = Verbose
  | Quiet
  deriving (Eq)

runEach :: Verbosity -> StmtsFn
runEach verbose modName stmts = f stmts
  where
    f [] = return ()
    f (stmt:rest) = do
      runStmt verbose modName stmt
      flush >>= printResults (null rest)
      f rest

runStmt :: Verbosity -> String -> Stmt -> Pipeline ()
runStmt verbose modName stmt = runAll verbose modName [stmt]

runAll :: Verbosity -> StmtsFn
runAll verbose modName stmts = do
  (nenv, rnEnv, tcState, dsState, env) <- getEnv
  dumpIR <- option dump_ir
  renameStmts modName stmts rnEnv \> \(rnEnv', renamed) ->
    reassocStmts renamed nenv \> \(nenv', rebalanced) ->
      inferStmts tcState rebalanced \> \(tcState', (typed, ty)) ->
        let (dsState', core) = desugarStmts dsState typed
            (env', val) = evalWithEnv env core
         in (if dumpIR
                then puts (PP.print core)
                else when (verbose == Verbose) (maybePrintTypedValue val ty)) >>
                  updateEnv (nenv', rnEnv', tcState', dsState', env')

maybePrintTypedValue :: (Show a, PP.PrettyPrint b) => a -> Maybe b -> Pipeline ()
maybePrintTypedValue _ Nothing = return ()
maybePrintTypedValue val (Just ty) = puts (typedValue val ty)
