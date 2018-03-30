module Typing.Expr (i_expr) where

import Typing.Constraint
import Typing.Ctx
import Typing.State
import {-# SOURCE #-} Typing.Stmt
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types
import Typing.Util

import Absyn.Base
import Absyn.Meta
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

import Control.Monad (foldM, when, zipWithM)
import Data.Bifunctor (first)
import Data.List (union)

i_expr :: Ctx -> U.Expr -> Tc (T.Expr, Type)
i_expr _ (Literal lit) = return (Literal lit, i_lit lit)

i_expr ctx (Ident [i] _) = do
  ty <- getValueType i ctx
  return (Ident [i] ty, ty)

-- TODO: Clear this up - should be handled by the renamer now
i_expr _ (Ident (_:_) _) = undefined
i_expr _ (Ident [] _) = undefined

i_expr ctx (ParenthesizedExpr expr) = i_expr ctx expr

i_expr ctx (BinOp _ _ lhs op rhs) = do
  tyOp@(Fun gen _ _) <- getValueType op ctx
  (lhs', lhsTy) <- i_expr ctx lhs
  (rhs', rhsTy) <- i_expr ctx rhs
  (retType', typeArgs)  <- inferTyArgs [lhsTy, rhsTy] tyOp
  constraintArgs <- inferConstraintArgs ctx gen [] typeArgs
  return (BinOp constraintArgs typeArgs lhs' (op, tyOp) rhs', retType')

i_expr ctx (Match expr cases) = do
  (expr', ty) <- i_expr ctx expr
  (cases', casesTy) <- unzip <$> mapM (i_case ctx ty) cases
  let retTy = case casesTy of
                [] -> void
                x:xs -> foldl (\/) x xs
  return (Match expr' cases', retTy)

i_expr ctx (Call fn constraintArgs types []) =
  i_expr ctx (Call fn constraintArgs types [VoidExpr])

i_expr ctx (Call fn _ types args) = do
  (fn', tyFn) <- i_expr ctx fn
  let tyFn' = normalizeFnType tyFn
  (tyFn''@(Fun gen _ retType), skippedVars) <- adjustFnType (null types) args tyFn'
  (retType', args', typeArgs)  <-
        case (tyFn'', types) of
          (Fun (_:_) params _, []) -> do
            (_, argsTy) <- unzip <$> mapM (i_expr ctx) args
            (retType, typeArgs) <- inferTyArgs argsTy tyFn''
            let s = zipSubst (map fst gen) typeArgs
            let params' = map (applySubst s) params
            args' <- zipWithM (instSubtype ctx) args params'
            return (retType, args', typeArgs)
          (Fun gen params _, _) -> do
            types' <- mapM (resolveType ctx) types
            let s = zipSubst (map fst gen) types'
            let params' = map (applySubst s) params
            args' <- zipWithM (instSubtype ctx) args params'
            return (applySubst s retType, args', types')
          _ -> undefined
  constraintArgs <- inferConstraintArgs ctx gen skippedVars typeArgs
  return (Call fn' constraintArgs typeArgs args', retType')

i_expr ctx (Record fields) = do
  (exprs, types) <- mapM (i_expr ctx . snd) fields >>= return . unzip
  let labels = map fst fields
  let fieldsTy = zip labels types
  let recordTy = Rec fieldsTy
  let record = Record (zip fieldsTy exprs)
  return (record, recordTy)

i_expr ctx (FieldAccess expr _ field) = do
  (expr', ty) <- i_expr ctx expr
  let
      aux :: Type -> [(String, Type)] -> Tc (T.Expr, Type)
      aux ty r = case lookup field r of
                Nothing -> throwError $ UnknownField ty field
                Just t -> return (FieldAccess expr' ty (field, t), t)
  case ty of
    Rec r -> aux ty r
    Cls _ -> do
      vars <- getInstanceVars ty ctx
      aux ty vars
    _ -> throwError . GenericError $ "Expected a record, but found value of type " ++ show ty

i_expr ctx (If ifCond ifBody elseBody) = do
  (ifCond', ty) <- i_expr ctx ifCond
  ty <:! bool
  (_, ifBody', ifTy) <- i_stmts ctx ifBody
  (_, elseBody', elseTy) <- i_stmts ctx elseBody
  return (If ifCond' ifBody' elseBody', ifTy \/ elseTy)

i_expr ctx (List _ items) = do
  (items', itemsTy) <- unzip <$> mapM (i_expr ctx) items
  (ty, itemTy) <- case itemsTy of
                    [] -> do
                      nilTy <- getValueType "Nil" ctx
                      return (nilTy, Bot)
                    x:xs ->
                      let ty = foldl (\/) x xs
                       in return (list ty, ty)
  return (List itemTy items', ty)

i_expr ctx (FnExpr fn) =
  first FnExpr <$> i_fn ctx fn

i_expr ctx (Negate _ expr) = do
  (expr', ty) <- i_expr ctx expr
  intf <- getInterface "Std.Number" ctx
  constrArgs <- boundsCheck ctx ty intf
  return (Negate constrArgs expr', ty)

-- Expressions generated during type checking
i_expr _ VoidExpr = return (VoidExpr, void)

i_expr _ (TypeCall {}) = undefined

instSubtype :: Ctx -> U.Expr -> Type -> Tc T.Expr
instSubtype ctx arg ty = do
  (arg', argTy) <- i_expr ctx arg
  arg'' <- case (argTy, ty) of
             (Fun gen@(_:_) _ _, Fun [] _ _) -> do
               typeArgs <- inferTyAbs argTy ty
               constraintArgs <- inferConstraintArgs ctx gen [] typeArgs
               return $ TypeCall arg' constraintArgs
             _ -> do
               argTy <:! ty
               return arg'
  return arg''

inferConstraintArgs :: Ctx -> [BoundVar] -> [BoundVar] -> [Type] -> Tc [ConstraintArg]
inferConstraintArgs ctx gen skippedVars typeArgs = do
  concat <$> zipWithM findConstrArgs gen typeArgs'
    where
      typeArgs' = zipWith findHoles gen typeArgs

      findHoles var ty =
        if var `elem` skippedVars
           then mkHole var
           else ty

      findConstrArgs (_, []) tyArg = do
        return [CAType tyArg]

      findConstrArgs (_, bounds) tyArg = do
        concat <$> mapM (boundsCheck ctx tyArg) bounds

boundsCheck :: Ctx -> Type -> Intf -> Tc [ConstraintArg]
boundsCheck ctx t1 t2@(Intf name _ _) = do
  args <- boundsCheck' ctx t1 t2
  if null args
     then throwError $ MissingImplementation name t1
     else return args

boundsCheck' :: Ctx -> Type -> Intf -> Tc [ConstraintArg]
boundsCheck' _ v@(Var _ bounds) intf = do
  return $ if intf `elem` bounds
              then [CABound v intf]
              else []

boundsCheck' ctx (TyApp ty args) intf@(Intf name _ _) = do
  implementations <- getImplementations name ctx
  case lookup ty implementations of
    Nothing -> return []
    Just vars -> do
      let aux arg (_, bounds) =
            concat <$> mapM (boundsCheck ctx arg) bounds
      args <- concat <$> zipWithM aux args vars
      return [CAPoly ty intf args]

boundsCheck' ctx (TyAbs params ty) intf =
  boundsCheck' ctx (params \\ ty) intf

boundsCheck' _ Bot intf =
  return [CABound Bot intf]

boundsCheck' ctx ty intf@(Intf name _ _) = do
  implementations <- getImplementations name ctx
  case lookup ty implementations of
    Just [] -> return [CABound ty intf]
    _ -> return []

normalizeFnType :: Type -> Type
normalizeFnType (Fun gen params (Fun [] params' retTy)) =
  normalizeFnType (Fun gen (params ++ params') retTy)
normalizeFnType ty = ty

adjustFnType :: Bool -> [a] -> Type -> Tc (Type, [BoundVar])
adjustFnType allowHoles args fn@(Fun gen params retType) = do
  let lArgs = length args
  case compare lArgs (length params) of
    EQ -> return (fn, [])
    LT ->
      let headArgs = take lArgs params
          tailArgs = drop lArgs params
          skippedGen = filter aux gen
          aux (v, _) = allowHoles && v `elem` (fv $ Fun [] tailArgs retType) && v `notElem` (foldl union [] $ map fv headArgs)
       in return (Fun gen headArgs $ Fun skippedGen tailArgs retType, skippedGen)
    GT -> throwError ArityMismatch
adjustFnType _ _ ty = throwError . GenericError $ "Expected a function, found " ++ show ty

i_lit :: Literal -> Type
i_lit (Integer _) = int
i_lit (Float _) = float
i_lit (Char _) = char
i_lit (String _) = string

i_case :: Ctx -> Type -> U.Case -> Tc (T.Case, Type)
i_case ctx ty (Case pattern caseBody) = do
  (pattern', ctx') <- c_pattern ctx ty pattern
  (_, caseBody', ty) <- i_stmts ctx' caseBody
  return (Case pattern' caseBody', ty)

-- TODO: Ctx should come first in the tuple
c_pattern :: Ctx -> Type -> U.Pattern -> Tc (T.Pattern, Ctx)
c_pattern ctx _ PatDefault = return (PatDefault, ctx)

c_pattern ctx ty (PatLiteral l) = do
  let litTy = i_lit l
  litTy <:! ty
  return (PatLiteral l, ctx)

c_pattern ctx ty (PatVar v) =
  let pat = PatVar (v, ty)
      ctx' = addValueType ctx (v, ty)
   in return (pat, ctx')

c_pattern ctx ty@(Rec tyFields) (PatRecord fields) = do
  (fields', ctx') <- foldM aux ([], ctx) fields
  return (PatRecord $ reverse fields', ctx')
    where
      aux :: ([(T.Id, T.Pattern)], Ctx) -> (Name, U.Pattern) -> Tc ([(T.Id, T.Pattern)], Ctx)
      aux (fields, ctx) (key, pat) = do
        case lookup key tyFields of
          Just ty -> do
            (pat', ctx') <- c_pattern ctx ty pat
            return (((key, ty), pat') : fields, ctx')
          Nothing ->
            throwError . GenericError $ "Matching against field `" ++ key ++ "`, which is not included in the type of the value being matched, `" ++ show ty ++ "`"

c_pattern _ ty (PatRecord _) = do
  throwError . GenericError $ "Using a record pattern, but value being matched has type `" ++ show ty ++ "`"

c_pattern ctx ty (PatList pats rest) = do
  itemTy <- getItemTy ty
  (pats', ctx') <- foldM (aux itemTy) ([], ctx) pats
  let (rest', ctx'') = case rest of
                         NoRest -> (NoRest, ctx')
                         DiscardRest -> (DiscardRest, ctx')
                         NamedRest n ->
                           let ctx'' = addValueType ctx' (n, ty)
                            in (NamedRest (n, ty), ctx'')
  return (PatList (reverse pats') rest', ctx'')
  where
    getItemTy (TyAbs _ (TyApp (Con "List") _)) =
      return Top

    getItemTy (TyApp (Con "List") [ty]) =
      return ty

    getItemTy _ =
      throwError . GenericError $ "Using a list pattern, but value being matched has type `" ++ show ty ++ "`"

    aux ty (pats, ctx) pat = do
      (pat', ctx') <- c_pattern ctx ty pat
      return (pat' : pats, ctx')

c_pattern ctx ty (PatCtor name vars) = do
  ctorTy <- getValueType name ctx
  let (fnTy, params, retTy) = case ctorTy of
                            fn@(Fun [] params retTy) -> (fn, params, retTy)
                            fn@(Fun gen params retTy) -> (fn, params, TyAbs (map fst gen) retTy)
                            t -> (Fun [] [] t, [], t)
  when (length vars /= length params) (throwError ArityMismatch)
  retTy <:! ty
  let substs = case (retTy, ty) of
                 (TyAbs gen _, TyApp _ args) -> zipSubst gen args
                 _ -> emptySubst
  let params' = map (applySubst substs) params
  (vars', ctx') <- foldM aux ([], ctx) (zip params' vars)
  return (PatCtor (name, fnTy) (reverse vars'), ctx')
    where
      aux (vars, ctx) (ty, var) = do
        (var', ctx') <- c_pattern ctx ty var
        return (var':vars, ctx')
