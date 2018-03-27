module Env
  ( Pipeline(..)
  , Env
  , runPipeline
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
import Typing.Ctx
import Util.Error

import qualified Interpreter.Env as Interpreter
import qualified Reassoc.Env as Reassoc
import qualified Renamer.Env as Renamer

import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (either)
import System.Exit (exitFailure)

type Env = (Reassoc.Env, Renamer.Env, Ctx, DsState, Interpreter.Env)

defaultEnv :: Env
defaultEnv =
  (Reassoc.defaultEnv, Renamer.defaultEnv, defaultCtx, initialState, Interpreter.defaultEnv)

data State = State
  { sOptions :: Options
  , sEnv :: Env
  , sErrors :: [Error]
  , sOut :: [String]
  }

initState :: Options -> State
initState options =
  State { sOptions = options
        , sEnv = defaultEnv
        , sErrors = []
        , sOut = []
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
  (s, _) <- runPipeline_ p (initState options)
  case sErrors s of
    [] -> mapM_ putStrLn (sOut s)
    errs -> mapM_ report errs >> exitFailure

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
  modify $ \s -> s { sErrors = sErrors s ++ errors}

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
