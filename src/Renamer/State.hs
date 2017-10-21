module Renamer.State
  ( Rn
  , runRn
  , thisMod
  ) where

import Util.Error

import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Except (Except, runExcept)

data RnState = RnState { modName :: String }

type Rn a = (StateT RnState (Except Error) a)

initRn :: String -> RnState
initRn modName = RnState { modName = modName }

runRn :: String -> Rn a -> Result a
runRn mod m =
  runExcept $ evalStateT m (initRn mod)

thisMod :: Rn String
thisMod =
  gets modName
