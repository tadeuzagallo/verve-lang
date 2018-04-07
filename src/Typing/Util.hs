module Typing.Util where

import Typing.Ctx
import Typing.Kinds
import Typing.State
import {-# SOURCE #-} Typing.Stmt
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types

import Util.Error

import Absyn.Base
import Absyn.Meta
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

import Control.Monad (when)
import Data.Maybe (fromMaybe)

(<:!) :: Type -> Type -> Tc ()
actualTy <:! expectedTy =
  when (not $ actualTy <: expectedTy) (throwError $ TypeError expectedTy actualTy)

assertKindStar :: Type -> Tc ()
assertKindStar ty =
  let kind = kindOf ty
   in when (kind /= Star) (throwError $ KindError ty Star kind)

resolveId :: U.Id -> Tc T.Id
resolveId (n, ty) = (,) n  <$> resolveType ty

resolveType :: U.Type -> Tc Type
resolveType (U.TName v) =
  getType v

resolveType (U.TArrow params ret) = do
  params' <- mapM resolveType params
  ret' <- resolveType ret
  return $ Fun [] params' ret'
resolveType (U.TRecord fieldsTy) = do
  fieldsTy' <- mapM resolveId fieldsTy
  return $ Rec fieldsTy'
resolveType (U.TApp t1 t2) = do
  t1' <- resolveType t1
  t2' <- mapM resolveType t2
  case t1' of
    TyAbs params ty -> do
      when (length params /= length t2) $ throwError TypeArityMismatch
      return $ applySubst (zipSubst params t2') ty
    _ -> return $ TyApp t1' t2'
resolveType U.TVoid = return void
resolveType U.TPlaceholder = undefined

resolveGenericBounds :: [(Name, [String])] -> Tc [(Name, [Intf])]
resolveGenericBounds gen =
  mapM resolve gen
    where
      resolve (name, bounds) = do
        bounds' <- mapM resolveConstraint bounds
        return (name, bounds')

      resolveConstraint intf = do
        getInterface intf

resolveGenericVars :: [(Name, [Intf])] -> Tc [BoundVar]
resolveGenericVars generics =
  mapM aux generics
    where
      aux (g, bounds) = do
        g' <- newVar g
        return (g', bounds)

addGenerics :: [BoundVar] -> Tc ()
addGenerics generics =
  mapM_ aux generics
    where
      aux (var, bounds) = do
        addType (varName var, Var var bounds)

defaultBounds :: [a] -> [(a, [b])]
defaultBounds = map (flip (,) [])

i_fn :: U.Function -> Tc (T.Function, Type)
i_fn fn = do
  gen' <- resolveGenericBounds (generics fn)
  genericVars <- resolveGenericVars gen'

  m <- startMarker
  addGenerics genericVars
  (ty, tyArgs, retType') <- fnTy (genericVars, params fn, retType fn)
  mapM_ addValueType tyArgs
  endMarker m

  addValueType (name fn, ty)
  (body', bodyTy) <- i_body (body fn)

  clearMarker m

  bodyTy <:! retType'

  let fn' = fn { name = (name fn, ty)
               , generics = gen'
               , params = tyArgs
               , retType = retType'
               , body = body'
               }
  return (fn', ty)

i_body :: [U.Stmt] -> Tc ([T.Stmt], Type)
i_body stmts = do
  m <- startMarker
  (stmts', ty) <- i_stmts stmts
  endMarker m
  clearMarker m
  return (stmts', fromMaybe void ty)

fnTy :: ([BoundVar], [(Name, U.Type)], U.Type) -> Tc (Type, [(Name, Type)], Type)
fnTy (generics, params, retType) = do
  tyArgs <- mapM resolveId params
  mapM_ (assertKindStar . snd) tyArgs
  retType' <- resolveType retType
  assertKindStar retType'
  let tyArgs' = if null tyArgs
      then [void]
      else map snd tyArgs
  let ty = Fun generics tyArgs' retType'
  return (ty, tyArgs, retType')

