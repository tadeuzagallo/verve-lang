module Typing.Util where

import Typing.Ctx
import Typing.Kinds
import Typing.State
import {-# SOURCE #-} Typing.Stmt
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types

import Absyn.Base
import Absyn.Meta
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

import Control.Monad (foldM, when)

(<:!) :: Type -> Type -> Tc ()
actualTy <:! expectedTy =
  when (not $ actualTy <: expectedTy) (throwError $ TypeError expectedTy actualTy)

assertKindStar :: Type -> Tc ()
assertKindStar ty =
  let kind = kindOf ty
   in when (kind /= Star) (throwError $ KindError ty Star kind)

resolveId :: Ctx -> U.Id -> Tc T.Id
resolveId ctx (n, ty) = (,) n  <$> resolveType ctx ty

resolveType :: Ctx -> U.Type -> Tc Type
resolveType ctx (U.TName v) =
  getType v ctx
resolveType ctx (U.TArrow params ret) = do
  params' <- mapM (resolveType ctx) params
  ret' <- resolveType ctx ret
  return $ Fun [] params' ret'
resolveType ctx (U.TRecord fieldsTy) = do
  fieldsTy' <- mapM (resolveId ctx) fieldsTy
  return $ Rec fieldsTy'
resolveType ctx (U.TApp t1 t2) = do
  t1' <- resolveType ctx t1
  t2' <- mapM (resolveType ctx) t2
  case t1' of
    TyAbs params ty -> do
      when (length params /= length t2) $ throwError TypeArityMismatch
      return $ applySubst (zipSubst params t2') ty
    _ -> return $ TyApp t1' t2'
resolveType _ U.TVoid = return void
resolveType _ U.TPlaceholder = undefined

resolveGenerics :: Ctx -> [(Name, [String])] -> Tc [(Name, [Intf])]
resolveGenerics ctx gen =
  mapM resolve gen
    where
      resolve (name, bounds) = do
        bounds' <- mapM resolveConstraint bounds
        return (name, bounds')

      resolveConstraint intf = do
        getInterface intf ctx

addGenerics :: Ctx -> [(Name, [Intf])] -> Tc (Ctx, [BoundVar])
addGenerics ctx generics =
  foldM aux (ctx, []) generics
    where
      aux (ctx, vars) (g, bounds) = do
        g' <- newVar g
        return (addType ctx (g, Var g' bounds), vars ++ [(g', bounds)])

defaultBounds :: [a] -> [(a, [b])]
defaultBounds = map (flip (,) [])

i_fn :: Ctx -> U.Function -> Tc (T.Function, Type)
i_fn = i_fnBase True

i_fnBase :: Bool -> Ctx -> U.Function -> Tc (T.Function, Type)
i_fnBase addToCtx ctx fn = do
  gen' <- resolveGenerics ctx $ generics fn
  (ctx', genericVars) <- addGenerics ctx gen'
  (ty, tyArgs, retType') <- fnTy ctx' (genericVars, params fn, retType fn)
  let ctx'' = if addToCtx
                then addValueType ctx' (name fn, ty)
                else ctx'
  let ctx''' = foldl addValueType ctx'' tyArgs
  (_, body', bodyTy) <- i_stmts ctx''' (body fn)
  bodyTy <:! retType'
  let fn' = fn { name = (name fn, ty)
               , generics = gen'
               , params = tyArgs
               , retType = retType'
               , body = body'
               }
  return (fn', ty)

fnTy :: Ctx -> ([BoundVar], [(Name, U.Type)], U.Type) -> Tc (Type, [(Name, Type)], Type)
fnTy ctx (generics, params, retType) = do
  tyArgs <- mapM (resolveId ctx) params
  mapM_ (assertKindStar . snd) tyArgs
  retType' <- resolveType ctx retType
  assertKindStar retType'
  let tyArgs' = if null tyArgs
      then [void]
      else map snd tyArgs
  let ty = Fun generics tyArgs' retType'
  return (ty, tyArgs, retType')

