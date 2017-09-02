module Typing.State
  ( Tc
  , initTc
  , runTc
  , mkUniqueId
  ) where

import Error
import Typing.TypeError

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Except (Except, runExcept)

type Tc a = (StateT TcState (Except TypeError) a)

data TcState = TcState { uid :: Int }

initTc :: TcState
initTc = TcState { uid = 0 }

runTc :: Tc a -> (a -> b) -> Result b
runTc m f =
  case runExcept $ evalStateT m initTc of
    Left err -> Left (Error err)
    Right v -> Right (f v)

mkUniqueId :: Tc Int
mkUniqueId = do
  s <- get
  put s{uid = uid s + 1}
  return $ uid s
