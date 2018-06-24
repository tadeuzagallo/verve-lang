{-# LANGUAGE MultiWayIf #-}
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
import Bytecode.Encoder
import Core.Desugar (desugarStmts)
import Interpreter.Eval (evalWithEnv)
import Reassoc.Reassoc (reassocStmts)
import Renamer.Renamer (renameStmts)
import Typing.TypeChecker (inferStmts)
import qualified Bytecode.Compiler as Bytecode
import qualified Util.PrettyPrint as PP

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromJust)
import Data.ByteString.Lazy as ByteString (writeFile)

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

  -- read options
  dumpIR <- option dump_ir
  dumpBytecode <- option dump_bytecode
  dumpSerializedBytecode <- option dump_serialized_bytecode

  renameStmts modName stmts rnEnv \> \(rnEnv', renamed) ->
    reassocStmts renamed nenv \> \(nenv', rebalanced) ->
      inferStmts tcState rebalanced \> \(tcState', (typed, ty)) ->
        let (dsState', core) = desugarStmts dsState typed
            (env', val) = evalWithEnv env core
         in (if | dumpIR -> puts (PP.print core)
                | dumpBytecode -> puts (PP.print (Bytecode.compile core))
                | isJust dumpSerializedBytecode ->
                  liftIO $ ByteString.writeFile (fromJust dumpSerializedBytecode) (encodeBytecode (Bytecode.compile core))
                | verbose == Verbose -> maybePrintTypedValue val ty
                | otherwise -> return ()
            ) >> updateEnv (nenv', rnEnv', tcState', dsState', env')

              where

maybePrintTypedValue :: (Show a, PP.PrettyPrint b) => a -> Maybe b -> Pipeline ()
maybePrintTypedValue _ Nothing = return ()
maybePrintTypedValue val (Just ty) = puts (typedValue val ty)
