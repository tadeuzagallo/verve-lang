module Typing.State
  ( Tc
  , TcState
  , initialState
  , runTc
  , mkUniqueId
  , throwError
  , startMarker
  , endMarker
  , clearMarker
  , importModule
  , getCtx
  , modifyCtx
  ) where

import {-# SOURCE #-} Typing.Ctx
import Typing.TypeError

import Util.Error

import Control.Monad (when)
import Control.Monad.State (StateT, runStateT, get, gets, modify, put)
import Control.Monad.Except (Except, runExcept)
import qualified Data.Map as Map
import Data.Maybe (isJust, maybe)

import qualified Control.Monad.Except as Except (throwError)

type Tc a = (StateT TcState (Except Error) a)

data TcState = TcState { uid :: Int
                       , ctx :: Ctx
                       , markers :: Map.Map MarkerId Marker
                       }

initialState :: TcState
initialState = TcState { uid = 0
                       , ctx = defaultCtx
                       , markers = Map.empty
                       }

runTc :: Tc a -> TcState -> Result (TcState, a)
runTc m state =
  case runExcept $ runStateT m state of
    Left err -> Left (Error err)
    Right (v, s) ->
      if not (Map.null $ markers s)
         then Left (Error $ GenericError "Type checking finished with uncleared markers")
         else Right (s, v)

mkUniqueId :: Tc Int
mkUniqueId = do
  s <- get
  put s{uid = uid s + 1}
  return $ uid s

throwError :: ErrorT a => a -> Tc b
throwError = Except.throwError . Error

-- Markers

newtype MarkerId = MarkerId Int deriving (Eq, Ord, Show)
data Marker = Marker { start :: Ctx, end :: Maybe Ctx }

startMarker :: Tc MarkerId
startMarker = do
  id <- MarkerId <$> mkUniqueId
  c <- gets ctx
  let marker = Marker { start = c, end = Nothing }
  modify $ \s -> s { markers = Map.insert id marker (markers s) }
  return id

endMarker :: MarkerId -> Tc ()
endMarker id = do
  marker <- getMarker id
  when (isJust $ end marker) $ throwError $ GenericError "Marker already ended"
  modify $ \s ->
    s { markers = Map.update (\m -> Just $ m { end = Just (ctx s) }) id (markers s) }

clearMarker :: MarkerId -> Tc ()
clearMarker markerId = do
  marker <- getMarker markerId
  c <- gets ctx
  e <- maybe unendedMarker return (end marker)
  let ctx = deleteBetween (start marker) e c
   in modify $ \s -> s { ctx, markers = Map.delete markerId (markers s) }
  where
    unendedMarker = throwError $ GenericError "Cannot clear marker that wasn't ended"

getMarker :: MarkerId -> Tc Marker
getMarker id = do
  ms <- gets markers
  maybe invalidMarker return $ Map.lookup id ms
    where
      invalidMarker = throwError $ GenericError "Unknown marker"

getCtx :: Tc Ctx
getCtx = gets ctx

modifyCtx :: (Ctx -> Ctx) -> Tc ()
modifyCtx f =
  modify $ \s -> s { ctx = f (ctx s) }

importModule :: [String] -> TcState -> TcState -> TcState
importModule items prevState impState =
  prevState { ctx = tImportModule items (ctx prevState) (ctx impState) }
