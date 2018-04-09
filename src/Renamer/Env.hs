module Renamer.Env
  ( Rn
  , RnName(..)
  , RnEnv
  , runRn
  , defaultEnv
  , local
  , insertLocalType
  , insertLocalValue
  , insertLocalInterface
  , insertInternalType
  , insertInternalValue
  , ident
  , renameType
  , renameValue
  , renameIdentValue
  , renameInterface
  , joinName
  -- for imports
  , findValuesAndTypes
  ) where

import Renamer.Error

import Lib.Registry
import Typing.TypeError (TypeError(ExtraneousImplementation, UnknownInterface))
import Util.Env
import Util.Error
import Util.Scope

import Data.List (intercalate)
import Data.Maybe (maybe)

-- Extra environment used for renaming
data RenamerEnv = RenamerEnv
  { modName :: String
  } deriving (Show)

data RnIntf = RnIntf RnName (Field RdrName RnName)
  deriving (Show)

instance Env RenamerEnv where
  type KeyType RenamerEnv = RdrName
  type ValueType RenamerEnv = RnName
  type InterfaceType RenamerEnv = RnIntf

  deleteBetween _ _ = id

-- Helper interface for the Rn monad
type Rn a = Scoped RenamerEnv a
type RnEnv = Scope RenamerEnv

runRn :: String -> Rn a -> RnEnv -> Result (RnEnv, a)
runRn modName rn state =
  runScoped rn' state
    where
      rn' = do
        updateEnv $ \e -> e { modName }
        rn

defaultEnv :: RnEnv
defaultEnv =
  createScope
    values
    types
    []
    (RenamerEnv { modName = "" })
  where
    values = fromRegistry isValue ++ fromRegistry isCtor
    types = fromRegistry isType
    fromRegistry pred = mkBuiltin . name <$> filter pred registry
    mkBuiltin n = ((Nothing, n), Internal n)

type RdrName = (Maybe String, String)

data RnName
  = External (String, String)
  | Internal String
  deriving (Show)


{-mkBuiltins :: [String] -> RenamerEnv-}

-- Helpers
local :: String -> Rn String
local name = do
  mod <- thisMod
  return $ joinName (mod, name)

localRdr :: String -> Rn (Maybe String, String)
localRdr name =
  return (Nothing, name)

external :: String -> Rn RnName
external name = do
  mod <- thisMod
  return $ External (mod, name)

internal :: String -> Rn RnName
internal = return . Internal

insertion :: (String -> Rn a) -> (String -> Rn b) -> (a -> b -> Rn ()) -> String -> Rn ()
insertion rdrName rnName insert name = do
  key <- rdrName name
  value <- rnName name
  insert key value

insertLocalType :: String -> Rn ()
insertLocalType = insertion localRdr external insertType

insertLocalValue :: String -> Rn ()
insertLocalValue = insertion localRdr external insertValue

insertLocalInterface :: String -> [String] -> Rn ()
insertLocalInterface intf methods = do
  intfRdr <- localRdr intf
  intfRn <- external intf
  methods' <- mapM mkMethod methods
  insertInterface intfRdr (RnIntf intfRn methods')
    where
      mkMethod str = (,) <$> localRdr str <*> external str

insertInternalType :: String -> Rn ()
insertInternalType = insertion localRdr internal insertType

insertInternalValue :: String -> Rn ()
insertInternalValue = insertion localRdr internal insertValue

ident :: [String] -> RdrName
ident [] = undefined
ident [x] = (Nothing, x)
ident xs =
  let mod = intercalate "." $ init xs
      name = last xs
   in (Just mod, name)

rnName :: RnName -> String
rnName (Internal n) = n
rnName (External n) = joinName n

lookupRnName :: (RdrName -> Rn (Maybe RnName)) -> RdrName -> Rn RnName
lookupRnName lookup rdrName = do
  maybeName <- lookup rdrName
  maybe unknown return maybeName
    where
      -- TODO: better errors here according to the type of the variable
      unknown = throwError $ UnknownVariable $ showRdrName rdrName

lookupRdrName :: (RdrName -> Rn (Maybe RnName)) -> RdrName -> Rn String
lookupRdrName f = fmap rnName . lookupRnName  f

renameType :: String -> Rn String
renameType name = lookupRdrName lookupType (Nothing, name)

renameValue :: String -> Rn String
renameValue name = lookupRdrName lookupValue (Nothing, name)

renameIdentValue :: [String] -> Rn String
renameIdentValue = lookupRdrName lookupValue . ident

renameInterface :: String -> Rn (String, String -> Rn String)
renameInterface name = do
  maybeIntf <- lookupInterface =<< localRdr name
  RnIntf name methods <- maybe unknownInterface return maybeIntf
  return (rnName name, renameIntfMethod methods)
    where
      unknownInterface = throwError $ UnknownInterface $ name

renameIntfMethod :: Field RdrName RnName -> String -> Rn String
renameIntfMethod methods method = do
  method' <- localRdr method
  case Prelude.lookup method' methods of
    Nothing -> unknownIntfMethod
    Just name -> return $ rnName name
  where
    unknownIntfMethod = throwError $ ExtraneousImplementation $ method

showRdrName :: RdrName -> String
showRdrName (Nothing, name) = name
showRdrName (Just mod, name) = joinName (mod, name)

joinName :: (String, String) -> String
joinName (mod, n) = mod ++ "." ++ n

-- HELPERS
thisMod :: Rn String
thisMod = getEnv modName

-- For imports
findValuesAndTypes :: String -> Maybe String -> RnEnv -> ([(RdrName, RdrName)], [(RdrName, RdrName)])
findValuesAndTypes modName modAlias env =
  (rdrNames values, rdrNames types ++ rdrNames interfaces)
  where
    valuesResult = runScoped (filterValues filter) env
    values = getValue valuesResult

    typesResult = runScoped (filterTypes filter) env
    types = getValue typesResult

    interfacesResult = runScoped (filterInterfaces filterIntf) env
    interfaces = getValue interfacesResult

    filter _ (External (m, _)) = m == modName
    filter _ _ = False

    filterIntf _ (RnIntf (External (m, _)) _) = m == modName
    filterIntf _ _ = False

    rdrNames :: [(RdrName, a)] -> [(RdrName, RdrName)]
    rdrNames = map $ \((_, name), _) -> ((Nothing, name), (modAlias, name))

    getValue (Right (_, a)) = a
    getValue _ = undefined
