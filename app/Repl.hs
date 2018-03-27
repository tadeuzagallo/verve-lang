module Repl (repl) where

import Compile
import Env
import Runners

import Syntax.Parser

import Control.Monad.Trans (lift)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Console.Haskeline.MonadException (MonadException, RunIO(..), controlIO)

instance MonadException Pipeline where
  controlIO f = Pipeline $ \s ->
    controlIO $ \(RunIO run) ->
      let run' = RunIO (fmap (Pipeline . const) . run . flip runPipeline_ s)
       in fmap (flip runPipeline_ s) $ f run'

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
              Right stmt -> runStmt "REPL" stmt
            lift (flush >>= printResults False)
            loop
