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
  , importDsState
  ) where

import qualified Core.Absyn as CA

import Absyn.Typed
import Lib.Registry
import Typing.Types

import Control.Monad.State (State, runState, gets, modify)
import Data.Tuple (swap)

data DsEnum = DsEnum { _enumName :: String, enumCtors :: [DsCtor] }
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
  , enums
  }
    where
      enums = map (uncurry DsEnum) $ foldl aux [] ctors

      ctors = filter isCtor registry

      aux enums ctor =
        let (name, ty) = decl ctor
            (enum, ctor') = mkDsCtor name ty
         in addTo enum ctor' enums

      mkDsCtor name (Fun _ args ty) =
        (getEnumForTy ty, DsCtor name (length args))
      mkDsCtor name ty = (getEnumForTy ty, DsCtor name 0)

      getEnumForTy (Con enum) = enum
      getEnumForTy (Forall _ ty) = getEnumForTy ty
      getEnumForTy (TyApp ty _) = getEnumForTy ty
      getEnumForTy _ = undefined

      addTo enum ctor [] = [(enum, [ctor])]
      addTo enum ctor ((enum', ctors):rest) | enum == enum' =
        (enum, ctor : ctors) : rest
      addTo enum ctor (x:xs) = x : addTo enum ctor xs



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
addEnum (Enum name _ ctors) =
  modify $ \s ->
    s { enums = DsEnum name ctors' : (enums s) }
  where
    ctors' = map mkCtor ctors
    mkCtor (name, args) = DsCtor { ctorName = name, arity = maybe 0 length args }

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

importDsState :: [String] -> DsState -> DsState -> DsState
importDsState imports targetState importState =
  importState { enums = mergedEnums }
    where
      mergedEnums = importedEnums ++ enums targetState
      importedEnums = filter pred (enums importState)
      pred (DsEnum name _) = name `elem` imports
