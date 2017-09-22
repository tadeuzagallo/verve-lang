module Typing.Variance
  ( Variance(..)
  , variance
  , invertVariance
  ) where

import Typing.Types

data Variance
  = Bivariant
  | Covariant
  | Contravariant
  | Invariant
  deriving (Eq, Show)

variance :: Var -> Type -> Variance
variance _ Top = Bivariant
variance _ Bot = Bivariant
variance _ (Con _) = Bivariant
variance _ Type = Bivariant
variance v (Var x _)
  | v == x = Covariant
  | otherwise = Bivariant
variance v (Fun x t r)
  | v `elem` map fst x = Bivariant
  | otherwise =
    let t' = map (invertVariance . variance v) t in
    (foldl joinVariance Bivariant t') `joinVariance` variance v r
variance v (Rec fields) =
  let vars = map (variance v . snd) fields
   in foldl joinVariance Bivariant vars
variance v (Cls _ vars) =
  let vars' = map (variance v . snd) vars
   in foldl joinVariance Bivariant vars'
variance v (TyAbs gen ty)
  | v `elem` gen = Bivariant
  | otherwise = variance v ty
variance v (TyApp ty args) =
  let vars = map (variance v) args
   in foldl joinVariance (variance v ty) vars

invertVariance :: Variance -> Variance
invertVariance Covariant = Contravariant
invertVariance Contravariant = Covariant
invertVariance c = c

joinVariance :: Variance -> Variance -> Variance
joinVariance Bivariant d = d
joinVariance c Bivariant = c
joinVariance c d | c == d = c
joinVariance _ _ = Invariant
