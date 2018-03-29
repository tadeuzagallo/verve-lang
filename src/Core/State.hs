module Core.State
  ( DsM
  , DsEnum
  , DsCtor
  , DsState
  , initialState
  , runDesugar
  , var
  , contVar
  , addEnum
  , findEnumForCtor
  , arity
  , enumCtors
  , ctorName
  ) where

import qualified Core.Absyn as CA

import Absyn.Typed
import Control.Monad.State (State, runState, gets, modify)
import Data.Tuple (swap)

data DsEnum = DsEnum { enumName :: String, enumCtors :: [DsCtor] }
data DsCtor = DsCtor { ctorName :: String, arity :: Int }
data DsState = DsState { contCount :: Int, varCount :: Int, enums :: [DsEnum] }

runDesugar :: (([CA.Var] -> DsM CA.Term) -> DsM CA.Term) -> DsState -> (DsState, CA.Term)
runDesugar d =
  let k z = return (CA.AppCont (CA.ContVar "halt") z)
   in swap . runState (d k)

initialState :: DsState
initialState = DsState
  { contCount = 0
  , varCount = 0
  -- TODO: this should live in the registry somehow
  , enums = [ DsEnum { enumName = "Bool"
                     , enumCtors = [ DsCtor "True" 0
                                   , DsCtor "False" 0
                                   ]
                     }
            , DsEnum { enumName = "List"
                     , enumCtors = [ DsCtor "Nil" 0
                                   , DsCtor "Cons" 2
                                   ]
                     }
            ]
  }

type DsM = State DsState

contVar :: DsM CA.ContVar
contVar = do
  count <- gets contCount
  modify $ \s -> s { contCount = count + 1 }
  return $ CA.ContVar ("#k" ++ show count)

var :: DsM CA.Var
var = do
  count <- gets varCount
  modify $ \s -> s { varCount = count + 1 }
  return $ CA.Var ("#x" ++ show count)

addEnum :: Decl -> DsM ()
addEnum (Enum (name, _) _ ctors) =
  modify $ \s ->
    s { enums = DsEnum name ctors' : (enums s) }
  where
    ctors' = map mkCtor ctors
    mkCtor ((name, _), args) = DsCtor { ctorName = name, arity = maybe 0 length args }

addEnum _ = error "addEnum called with a Decl that is not an enum"

findEnumForCtor :: String -> DsM DsEnum
findEnumForCtor c = do
  es <- gets enums
  return $ f es
    where
      f :: [DsEnum] -> DsEnum
      f [] = undefined
      f (e:es) =
        if g (enumCtors e) then e else f es
      g :: [DsCtor] -> Bool
      g [] = False
      g (c' : cs)
        | ctorName c' == c = True
        | otherwise = g cs
