module Typing.Substitution
  (Substitution
  , emptySubst
  , mkSubst
  , mkSubsts
  , zipSubst
  , applySubst
  , instantiate
  ) where

import Typing.Types

import Util.Scope

newtype Substitution = Substitution [(Var, Type)]

emptySubst :: Substitution
emptySubst = Substitution []

mkSubst :: (Var, Type) -> Substitution
mkSubst s = Substitution [s]

mkSubsts :: [(Var, Type)] -> Substitution
mkSubsts = Substitution

zipSubst :: [Var] -> [Type] -> Substitution
zipSubst vs = Substitution . zip vs

applySubst :: Substitution -> Type -> Type
applySubst _ Top = Top
applySubst _ Bot = Bot
applySubst _ (Con c) = Con c
applySubst _ (Cls c) = Cls c
applySubst (Substitution s) var@(Var v _) =
  case lookup v s of
    Nothing -> var
    Just t -> t
applySubst s (Fun gs t1 t2) =
  let gs' = map fst gs
      s' = filterSubst (not . flip elem gs') s
   in Fun gs (map (applySubst s') t1) (applySubst s' t2)
applySubst s (Rec fields) =
  Rec (map (\(k, v) -> (k, applySubst s v)) fields)
applySubst s (Forall gen ty) =
  let s' = filterSubst (not . flip elem gen) s
   in Forall gen (applySubst s' ty)
applySubst s (TyAbs gen ty) =
  let s' = filterSubst (not . flip elem gen) s
   in TyAbs gen (applySubst s' ty)
applySubst s (TyApp t1 t2) =
  TyApp (applySubst s t1) (map (applySubst s) t2)

filterSubst :: (Var -> Bool) -> Substitution -> Substitution
filterSubst f (Substitution s) =
  Substitution $ filter (f . fst) s

freshBound :: Env t => BoundVar -> Scoped t BoundVar
freshBound (var, bounds) = do
  var' <- freshVar var
  return (var', bounds)

instantiate :: Env t => Type -> Scoped t Type
instantiate (TyAbs gen ty) = do
  gen' <- mapM freshVar gen
  let s = zipSubst gen (map (flip Var []) gen')
  return $ TyAbs gen' (applySubst s ty)
instantiate (Fun gen params ret) = do
  gen' <- mapM freshBound gen
  let s = zipSubst (map fst gen) (map (uncurry Var) gen')
  return $ Fun gen' (map (applySubst s) params) (applySubst s ret)
instantiate ty = return ty
