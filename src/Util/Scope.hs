{-# LANGUAGE Rank2Types, StandaloneDeriving #-}
module Util.Scope
  ( Scoped
  , Scope
  , createScope
  , runScoped

  , lookupValue
  , lookupType
  , lookupInterface

  , insertValue
  , insertType
  , insertInterface

  , getEnv
  , updateEnv
  , lookupEnv
  , insertEnv

  , mkUniqueId

  , startMarker
  , endMarker
  , clearMarker

  -- to support imports
  , filterValues
  , filterTypes
  ) where

import Util.Env
import Util.Error

import Typing.TypeError

import Control.Monad (when)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, get, gets, modify, put, runStateT)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Tuple (swap)

-- Scoped monad
type Scoped env a = StateT (Scope env) (Except Error) a

runScoped :: Env env => Scoped env a -> Scope env -> Result (Scope env, a)
runScoped scoped scope =
  swap <$> runExcept (runStateT scoped scope)

-- Actual Scope
type Types env = Field (KeyType env) (ValueType env)
type Values env = Field (KeyType env) (ValueType env)
type Interfaces env = Field (KeyType env) (InterfaceType env)

data Scope env = Scope
  { values :: Values env
  , types :: Types env
  , interfaces :: Interfaces env
  , env :: env

  -- Meta data
  , markers :: Map.Map MarkerId (Marker env)
  , uid :: Int
  }

deriving instance Env t => Show (Scope t)

instance Env t => Env (Scope t) where
  type KeyType (Scope t) = KeyType t
  type ValueType (Scope t) = ValueType t
  type InterfaceType (Scope t) = InterfaceType t

  deleteBetween from to current =
    current { values     = deleteBetweenField (values from) (values to) (values current)
            , types      = deleteBetweenField (types from) (types to) (types current)
            , env = deleteBetween (env from) (env to) (env current)
            }

createScope :: Env t
            => Values t
            -> Types t
            -> Interfaces t
            -> t
            -> Scope t
createScope values types interfaces env =
  Scope { values
        , types
        , interfaces
        , env
        , markers = Map.empty
        , uid = 0
        }

lookupScope :: (Eq a, Env t) => (Scope t -> Field a b) -> a -> Scoped t (Maybe b)
lookupScope proj key = do
  scope <- get
  return $ Util.Env.lookup proj key scope

insertScope :: (Eq a, Env t) => (Scope t -> Field a b) -> (Scope t -> Field a b -> Scope t) -> a -> b -> Scoped t ()
insertScope proj inj key value = do
  scope <- get
  put (Util.Env.insert proj inj key value scope)

type Lookup m = forall t. (Env t, Eq (KeyType t)) => KeyType t -> Scoped t (Maybe (m t))
type Insertion m = forall t. (Env t, Eq (KeyType t)) => KeyType t -> m t -> Scoped t ()

lookupValue :: Lookup ValueType
lookupValue = lookupScope values

lookupType :: Lookup ValueType
lookupType = lookupScope types

lookupInterface :: Lookup InterfaceType
lookupInterface = lookupScope interfaces

insertValue :: Insertion ValueType
insertValue = insertScope values (\st values -> st { values })

insertType :: Insertion ValueType
insertType = insertScope types (\st types -> st { types })

insertInterface :: Insertion InterfaceType
insertInterface = insertScope interfaces (\st interfaces -> st { interfaces })


getEnv :: Env t => (t -> a) -> Scoped t a
getEnv proj = do
  proj <$> gets env

updateEnv :: Env t => (t -> t) -> Scoped t ()
updateEnv f = do
  modify $ \s -> s { env = f (env s) }

lookupEnv :: (Eq a, Env t) => (t -> Field a b) -> a -> Scoped t (Maybe b)
lookupEnv proj key = do
  lookupScope (proj . env) key

insertEnv :: (Eq a, Env t) => (t -> Field a b) -> (t -> Field a b -> t) -> a -> b -> Scoped t ()
insertEnv proj inj key value = do
  scope <- get
  let env' = inj (env scope) ((key, value) : proj (env scope))
   in put $ scope { env = env' }

-- Unique Id

mkUniqueId :: Env t => Scoped t Int
mkUniqueId = do
  id <- gets uid
  modify $ \s -> s { uid = id + 1 }
  return $ id


-- Markers

newtype MarkerId = MarkerId Int deriving (Eq, Ord, Show)
data Marker t =
  Marker { start :: Scope t
         , end :: Maybe (Scope t)
         } deriving (Show)

startMarker :: Env t => Scoped t MarkerId
startMarker = do
  id <- MarkerId <$> mkUniqueId
  c <- get
  let marker = Marker { start = c, end = Nothing }
  modify $ \s -> s { markers = Map.insert id marker (markers s) }
  return id

endMarker :: Env t => MarkerId -> Scoped t ()
endMarker id = do
  scope <- get
  marker <- getMarker id
  when (isJust $ end marker) $ throwError $ GenericError "Marker already ended"
  modify $ \s ->
    s { markers = Map.update (\m -> Just $ m { end = Just scope }) id (markers s) }

clearMarker :: Env t => MarkerId -> Scoped t ()
clearMarker markerId = do
  marker <- getMarker markerId
  c <- get
  e <- maybe unendedMarker return (end marker)
  let scope = deleteBetween (start marker) e c
   in put $ scope { markers = Map.delete markerId (markers scope) }
  where
    unendedMarker = throwError $ GenericError "Cannot clear marker that wasn't ended"

getMarker :: Env t => MarkerId -> Scoped t (Marker t)
getMarker id = do
  ms <- gets markers
  maybe invalidMarker return $ Map.lookup id ms
    where
      invalidMarker = throwError $ GenericError "Unknown marker"

-- Imports
type Filter m = forall t. Env t => (KeyType t -> m t -> Bool) -> Scoped t (Field (KeyType t) (m t))

filterScope :: Env t => (Scope t -> Field a b) -> (a -> b -> Bool) -> Scoped t (Field a b)
filterScope proj pred = do
  scope <- get
  return $ filter (uncurry pred) (proj scope)

filterValues :: Filter ValueType
filterValues = filterScope values

filterTypes :: Filter ValueType
filterTypes = filterScope types
