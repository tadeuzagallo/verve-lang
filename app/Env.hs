module Env
  ( Pipeline(..)
  , Env
  , defaultEnv
  , runPipeline
  , printResults
  , option
  , options
  , reportErrors
  , puts
  , updateEnv
  , getEnv
  , flush
  , (|>)
  , (\>)
  ) where

import Options

import Core.Desugar
import Util.Error

import qualified Interpreter.Env as Interpreter
import qualified Reassoc.Env as Reassoc
import qualified Renamer.Env as Renamer
import qualified Typing.State as Typing

import Control.Monad (ap, liftM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (either)
import System.Console.Haskeline.MonadException (MonadException, RunIO(..), controlIO)
import System.Exit (exitFailure)

type Env = (Reassoc.Env, Renamer.RnEnv, Typing.TcState, DsState, Interpreter.Env)

defaultEnv :: Env
defaultEnv =
  (Reassoc.defaultEnv, Renamer.defaultEnv, Typing.initialState, initialState, Interpreter.defaultEnv)

data State = State
  { sOptions :: Options
  , sEnv :: Env
  , sErrors :: [Error]
  , sOut :: [String]
  , sHasError :: Bool
  }

initState :: Options -> State
initState options =
  State { sOptions = options
        , sEnv = defaultEnv
        , sErrors = []
        , sOut = []
        , sHasError = False
        }

newtype Pipeline a = Pipeline
  { runPipeline_ :: State -> IO (State, a) }

instance Functor Pipeline where
  fmap = liftM

instance Applicative Pipeline where
  pure = return
  (<*>) = ap

instance Monad Pipeline where
  return a =
    Pipeline { runPipeline_ = \s -> return (s, a) }

  p >>= f =
    Pipeline { runPipeline_ = \state ->
      runPipeline_ p state >>= \(s1, a) ->
        runPipeline_ (f a) s1 }

instance MonadIO Pipeline where
  liftIO f =
    Pipeline { runPipeline_ = \s ->
      f >>= \v -> return (s, v) }

instance MonadException Pipeline where
  controlIO f = Pipeline $ \s ->
    controlIO $ \(RunIO run) ->
      let run' = RunIO (fmap (Pipeline . const) . run . flip runPipeline_ s)
       in fmap (flip runPipeline_ s) $ f run'

-- Internal Helpers
ask :: (State -> t) -> Pipeline t
ask f =
  Pipeline { runPipeline_ = \s -> return (s, f s) }

modify :: (State -> State) -> Pipeline ()
modify f =
  Pipeline { runPipeline_ = \s -> return (f s, ()) }

-- Public Helpers

runPipeline :: Pipeline a -> Options -> IO ()
runPipeline p options = do
  _ <- runPipeline_ (p >> flush >>= printResults True) (initState options)
  return ()

printResults :: Bool -> ([Error], [String]) -> Pipeline ()
printResults exitOnFailure ([], out) = do
  liftIO $ mapM_ putStrLn out
  hasError <- ask sHasError
  liftIO $ when (exitOnFailure && hasError) exitFailure

printResults exitOnFailure (errors, _) = do
  liftIO $ mapM_ report errors
  liftIO $ when exitOnFailure exitFailure

getEnv :: Pipeline Env
getEnv =
  ask sEnv

options :: Pipeline Options
options =
  ask sOptions

option :: (Options -> a) -> Pipeline a
option f =
  options >>= return . f

reportErrors :: [Error] -> Pipeline ()
reportErrors errors =
  modify $ \s -> s { sErrors = sErrors s ++ errors, sHasError = True }

puts :: String -> Pipeline ()
puts str =
  modify $ \s -> s { sOut = sOut s ++ [str]}

updateEnv :: Env -> Pipeline ()
updateEnv env =
  modify $ \s -> s { sEnv = env}

flush :: Pipeline ([Error], [String])
flush = do
  out <- ask sOut
  errors <- ask sErrors
  modify $ \s -> s { sOut = [], sErrors = [] }
  return (errors, out)

-- pipeline operator
(|>) :: Either [Error] a -> (a -> Pipeline ()) -> Pipeline ()
r |> f = either reportErrors f r

(\>) :: Either Error a -> (a -> Pipeline ()) -> Pipeline ()
r \> f = either (reportErrors . (:[])) f r
