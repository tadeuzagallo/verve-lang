module Typing.State
  ( Tc
  , initTc
  , runTc
  , mkUniqueId
  , throwError
  ) where

import Util.Error

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Except (Except, runExcept)

import qualified Control.Monad.Except as Except (throwError)

type Tc a = (StateT TcState (Except Error) a)

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

throwError :: ErrorT a => a -> Tc b
throwError = Except.throwError . Error
