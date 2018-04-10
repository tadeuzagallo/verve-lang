module Repl (repl) where

import Compile
import Env
import Runners

import Syntax.Parser

import Control.Monad.Trans (lift)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

repl :: Pipeline ()
repl = do
  loadPrelude
  runInputT defaultSettings $ loop
    where
      loop :: InputT Pipeline ()
      loop = do
        minput <- getInputLine "> "
        case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just "" -> outputStrLn "" >> loop
          Just input -> do
            lift $ case parseStmt "(stdin)" input of
              Left err -> reportErrors [err]
              Right stmt -> runStmt Verbose "REPL" stmt
            lift (flush >>= printResults False)
            loop
