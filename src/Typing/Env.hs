module Typing.Env
  ( Tc
  , TcEnv
  , defaultEnv
  , insertImplementation
  , lookupImplementations
  , insertInstanceVars
  , lookupInstanceVars

  -- Lookup wrappers
  , lookupValue
  , lookupType
  , lookupInterface

  -- Imports
  ,  importModule

  , module Util.Scope
  ) where

import Typing.Substitution
import Typing.TypeError
import Typing.Types

import Lib.Registry
import Util.Error
import Util.Scope hiding (lookupValue, lookupType, lookupInterface)
import qualified Util.Scope

import Data.Maybe (maybe)

data TypingEnv = TypingEnv
  { implementations :: [(String, [(Type, [BoundVar])])]
  , instanceVars :: [(Type, [(String, Type)])]
  } deriving (Show, Eq)

instance Env TypingEnv where
  type KeyType TypingEnv = String
  type ValueType TypingEnv = Type
  type InterfaceType TypingEnv = Intf

  deleteBetween from to current =
    -- implementations are always global
    current { instanceVars = deleteBetweenField (instanceVars from) (instanceVars to) (instanceVars current)
            }

type Tc a = Scoped TypingEnv a
type TcEnv = Scope TypingEnv

defaultEnv :: TcEnv
defaultEnv =
  createScope
    (decl <$> filter (\x -> isValue x || isCtor x) registry)
    (decl <$> filter isType registry)
    []
    (TypingEnv { implementations = []
               , instanceVars = []
               })

lookupImplementations :: String -> Tc [(Type, [BoundVar])]
lookupImplementations intfName = do
  maybeImpl <- lookupEnv implementations intfName
  maybe unknown return maybeImpl
  where
    unknown = throwError $ UnknownInterface intfName

lookupInstanceVars :: Type -> Tc [(String, Type)]
lookupInstanceVars cls@(Cls name) = do
  maybeVars <- lookupEnv instanceVars cls
  maybe unknown return maybeVars
  where
    unknown = throwError $ UnknownType name

lookupInstanceVars _ = undefined

insertImplementation :: String -> (Type, [BoundVar]) -> Tc ()
insertImplementation name inst = do
  updateEnv $ \env -> env { implementations = insert name inst (implementations env) }
  where
    insert key value [] = [(key, [value])]
    insert key value ((k,vs):rest) | k == key = (key, value : vs) : rest
    insert key value (x:xs) = x : insert key value xs

insertInstanceVars :: Type -> [(String, Type)] -> Tc ()
insertInstanceVars =
  insertEnv instanceVars $ \env instanceVars -> env { instanceVars }

-- Lookup wrappers

type Lookup a = String -> Tc a

lookupWrapper :: (String -> Tc (Maybe a))
              -> (String -> TypeError)
              -> (a -> Tc b)
              -> Lookup b
lookupWrapper lookup err f k =
  lookup k >>= maybe (throwError $ err k) f

lookupValue, lookupType :: Lookup Type
lookupValue = lookupWrapper Util.Scope.lookupValue UnknownVariable instantiate
lookupType = lookupWrapper Util.Scope.lookupType UnknownType instantiate

lookupInterface :: Lookup Intf
lookupInterface = lookupWrapper Util.Scope.lookupInterface UnknownInterface return


-- MODULE IMPORTATION
importModule :: [String] -> TcEnv -> TcEnv -> TcEnv
importModule items targetEnv impEnv =
  let items' = map (\i -> (i, i)) items
      e1 = foldl (importType impEnv) targetEnv items'
      e2 = foldl (importValue impEnv) e1 items'
      e3 = importEnv importer e2 impEnv
   in e3
 where
   importer targetEnv impEnv  =
     TypingEnv { implementations = mergeImplementations (implementations targetEnv) (implementations impEnv)
                -- TODO: filter in order to avoid overwriting
               , instanceVars = instanceVars impEnv ++ instanceVars targetEnv
               }

   mergeImplementations prev [] = prev
   mergeImplementations prev ((intf, impls): rest) =
     mergeImplementations (update prev intf impls) rest

   update [] intf impls = [(intf, impls)]
   update ((intf', impls') : rest) intf impls
     | intf == intf' = (intf, impls ++ impls') : rest
     | otherwise = (intf', impls') : update rest intf impls
