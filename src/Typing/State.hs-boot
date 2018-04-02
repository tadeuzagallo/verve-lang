module Typing.State where

import Util.Error
import Control.Monad.Except (Except)
import Control.Monad.State (StateT)

data TcState

type Tc a = (StateT TcState (Except Error) a)

mkUniqueId :: Tc Int
