{-# LANGUAGE Rank2Types, StandaloneDeriving #-}
module Util.Scope
  ( Scoped
  , Scope
  , createScope
  , runScoped

  , Env(..)
  , deleteBetweenField

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
  , filterInterfaces
  , importType
  , importValue
  , importEnv
  ) where

import Util.Env
import Util.Error

import Control.Monad (when)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, get, gets, modify, put, runStateT)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

-- Scoped monad
type Scoped env a = StateT (Scope env) (Except Error) a

runScoped :: Env env => Scoped env a -> Scope env -> Result (Scope env, a)
runScoped scoped scope =
  case runExcept $ runStateT scoped scope of
    Left err -> Left err
    Right (v, s) ->
      if not (Map.null $ markers s)
         then Left (Error $ GenericError "Type checking finished with uncleared markers")
         else Right (s, v)

-- Actual Scope
data TypeEntry env
  = Type (ValueType env)
  | Intf (InterfaceType env)

deriving instance Env env => Show (TypeEntry env)

unType :: Env t => TypeEntry t -> Maybe (ValueType t)
unType (Type t) = Just t
unType (Intf _) = Nothing

unIntf :: Env t => TypeEntry t -> Maybe (InterfaceType t)
unIntf (Intf t) = Just t
unIntf (Type _) = Nothing

type Types env = Field (KeyType env) (ValueType env)
type Values env = Field (KeyType env) (ValueType env)
type Interfaces env = Field (KeyType env) (InterfaceType env)

data Scope env = Scope
  { values :: Values env
  , types :: Field (KeyType env) (TypeEntry env)
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
        , types = typesAndInterfaces
        , env
        , markers = Map.empty
        , uid = 0
        }

 where
   typesAndInterfaces = map (fmap Type) types ++ map (fmap Intf) interfaces

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
lookupType key = do
  ty <- lookupScope types key
  return $ ty >>= unType

lookupInterface :: Lookup InterfaceType
lookupInterface key = do
  ty <- lookupScope types key
  return $ ty >>= unIntf

insertValue :: Insertion ValueType
insertValue = insertScope values (\st values -> st { values })

insertType :: Insertion ValueType
insertType key = insertScope types (\st types -> st { types }) key . Type

insertInterface :: Insertion InterfaceType
insertInterface key = insertScope types (\st types -> st { types }) key . Intf


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
filterTypes pred = do
  map (fmap (fromJust . unType)) <$> filterScope types (\k v ->
    case v of
      Type t -> pred k t
      _ -> False)

filterInterfaces :: Filter InterfaceType
filterInterfaces pred =
  map (fmap (fromJust . unIntf)) <$> filterScope types (\k v ->
    case v of
      Intf i -> pred k i
      _ -> False)

type Importer t = Scope t -> Scope t -> (KeyType t, KeyType t) -> Scope t

importScope :: (Env t, Eq (KeyType t))
            => (Scope t -> Field (KeyType t) b)
            -> (Scope t -> Field (KeyType t) b -> Scope t)
            -> Importer t
importScope proj inj importScope targetScope (lookupKey, insertionKey) =
  -- TODO: Handle errors
  let value = Util.Env.lookup proj lookupKey importScope
   in insert proj inj insertionKey (fromJust value) targetScope

importValue :: (Env t, Eq (KeyType t)) => Importer t
importValue = importScope values (\s values -> s { values })

importType :: (Env t, Eq (KeyType t)) => Importer t
importType = importScope types (\s types -> s { types })

importEnv :: (Env t, Eq (KeyType t))
          => (t -> t -> t)
          -> Scope t
          -> Scope t
          -> Scope t
importEnv importer targetScope importScope =
  let env' = importer (env targetScope) (env importScope)
   in targetScope { env = env' }
