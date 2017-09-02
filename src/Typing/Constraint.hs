module Typing.Constraint
  ( inferTyArgs
  ) where

import Typing.State
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types
import Typing.Variance

import Control.Monad (zipWithM)
import Control.Monad.Except (throwError)
import Data.List (groupBy, intersect, sortBy, union)
import Data.Maybe (fromJust)

-- Inference of type arguments for generic functions
inferTyArgs :: [Type] -> Type -> Tc (Type, [Type])
inferTyArgs args (Fun gen params retType) = do
  let vars = map fst gen
  let initialCs = map (flip (Constraint Bot) Top) vars
  cs <- zipWithM (constraintGen [] vars) args params
  let cs' = initialCs `meet` foldl meet [] cs
  substs <- mapM (getSubst retType) cs'
  let typeArgs = map (fromJust . flip lookup substs . fst) gen
  return (applySubst (mkSubsts substs) retType, typeArgs)

inferTyArgs _ _ = undefined

-- Constraint Solving
data Constraint
  = Constraint Type Var Type
  deriving (Eq, Show)

constraintGen :: [Var] -> [Var] -> Type -> Type -> Tc [Constraint]

-- CG-Top
constraintGen _ _ _ Top = return []

-- CG-Bot
constraintGen _ _ Bot _ = return []

-- CG-Upper
constraintGen v x (Var y _) s | y `elem` x && fv s `intersect` x == [] =
  let t = v \\ s
   in return [Constraint Bot y t]

-- CG-Lower
constraintGen v x s (Var y _) | y `elem` x && fv s `intersect` x == [] =
  let t = v // s
   in return [Constraint t y Top]

-- CG-Refl
constraintGen _v _x t1 t2 | t1 <: t2 = return []

-- CG-Fun
constraintGen v x (Fun y r s) (Fun y' t u)
  | y == y' && map fst y `intersect` (v `union` x) == [] = do
    c <- zipWithM (constraintGen (v `union` map fst y) x) t r
    d <- constraintGen (v `union` map fst y) x s u
    return $ foldl meet [] c `meet` d

constraintGen v x (TyApp t11 t12) (TyApp t21 t22) = do
  cTy <- constraintGen v x t11 t21
  cArgs <- zipWithM (constraintGen v x) t12 t22
  return $ foldl meet [] cArgs `meet` cTy

constraintGen v x (Rec f1) (Rec f2) | map fst f2 `intersect` map fst f1 == map fst f2 = do
  cs <- mapM aux f2
  return $ foldl meet [] cs
    where
      aux (key, v2) =
        constraintGen v x (fromJust $ lookup key f1) v2

constraintGen _v _x actual expected =
  throwError $ TypeError expected actual


-- The meet of two X/V-constraints C and D, written C /\ D, is defined as follows:
meet :: [Constraint] -> [Constraint] -> [Constraint]
meet c [] = c
meet [] d = d
meet c d =
  map merge cs
    where
      cs = groupBy prj sorted
      sorted = sortBy (\(Constraint _ t _) (Constraint _ u _) -> compare t u) (c `union` d)
      prj (Constraint _ t _) (Constraint _ u _) = t == u
      merge [] = undefined
      merge (c:cs) = foldl mergeC c cs
      mergeC (Constraint s x t) (Constraint u _ v) =
        Constraint (s \/ u) x (t /\ v)

getSubst :: Type -> Constraint -> Tc (Var, Type)
getSubst r (Constraint s x t) =
  case variance x r of
    Bivariant -> return (x, s)
    Covariant -> return (x, s)
    Contravariant -> return (x, t)
    Invariant | s == t -> return (x, s)
    _ -> throwError InferenceFailure
