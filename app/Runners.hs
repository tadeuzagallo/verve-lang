module Runners
  ( StmtsFn
  , runEach
  , runAll
  , runStmt
  ) where

import Env
import Options
import TypedValue

import Absyn.Untyped (Stmt)
import Core.Desugar (desugarStmt, desugarStmts)
import Interpreter.Eval (evalWithEnv)
import Reassoc.Reassoc (reassocStmt, reassocStmts)
import Renamer.Renamer (renameStmt, renameStmts)
import Typing.TypeChecker (inferStmt, inferStmts)

import qualified Util.PrettyPrint as PP

type StmtsFn = String -> [Stmt] -> Pipeline ()

runEach :: StmtsFn
runEach modName stmts = f stmts
  where
    f [] = return ()
    f (stmt:rest) = do
      runStmt modName stmt
      flush >>= printResults (null rest)
      f rest

runStmt :: String -> Stmt -> Pipeline ()
runStmt modName stmt = do
  (nenv, rnEnv, ctx, dsState, env) <- getEnv
  renameStmt modName rnEnv stmt \> \(rnEnv', renamed) ->
    reassocStmt nenv renamed \> \(nenv', rebalanced) ->
      inferStmt ctx rebalanced \> \(ctx', typed, ty) ->
        let (dsState', core) = desugarStmt dsState typed
            (env', val) = evalWithEnv env core
         in puts (typedValue val ty) >>
           updateEnv (nenv', rnEnv', ctx', dsState', env')

runAll :: StmtsFn
runAll modName stmts = do
  (nenv, rnEnv, ctx, dsState, env) <- getEnv
  dumpIR <- option dump_ir
  renameStmts modName rnEnv stmts \> \(rnEnv', renamed) ->
    reassocStmts nenv renamed \> \(nenv', rebalanced) ->
      inferStmts ctx rebalanced \> \(ctx', typed, ty) ->
        let (dsState', core) = desugarStmts dsState typed
            (env', val) = evalWithEnv env core
         in puts (if dumpIR
                     then PP.print core
                     else typedValue val ty) >>
                       updateEnv (nenv', rnEnv', ctx', dsState', env')
