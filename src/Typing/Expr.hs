module Typing.Expr (i_expr) where

import Typing.Constraint
import Typing.Env
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types
import Typing.Util

import Absyn.Base
import Absyn.Meta
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T
import Util.Error

import Control.Monad (when, zipWithM)
import Data.List (union)

i_expr :: U.Expr -> Tc (T.Expr, Type)
i_expr (meta :< Literal lit) =
  return (meta :< Literal lit, i_lit lit)

i_expr (meta :< Ident [i]) = do
  ty <- lookupValue i
  return (meta :< Ident [i], ty)

-- TODO: Clear this up - should be handled by the renamer now
i_expr (_ :< Ident (_:_)) = undefined
i_expr (_ :< Ident []) = undefined

i_expr (meta :< ParenthesizedExpr expr) = do
  (expr', ty) <- i_expr expr
  return (meta :< ParenthesizedExpr expr', ty)

i_expr (meta :< BinOp _ _ lhs op rhs) = do
  tyOp@(Fun gen _ _) <- lookupValue op
  (lhs', lhsTy) <- i_expr lhs
  (rhs', rhsTy) <- i_expr rhs
  (retType', typeArgs)  <- inferTyArgs [lhsTy, rhsTy] tyOp
  constraintArgs <- inferConstraintArgs gen [] typeArgs
  return (meta :< BinOp constraintArgs [] lhs' (op, tyOp) rhs', retType')

i_expr (meta :< Match expr cases) = do
  (expr', ty) <- i_expr expr
  (cases', casesTy) <- unzip <$> mapM (i_case ty) cases
  let retTy = case casesTy of
                [] -> void
                x:xs -> foldl (\/) x xs
  return (meta :< Match expr' cases', retTy)

i_expr (meta :< Call fn constraintArgs types []) =
  i_expr (meta :< Call fn constraintArgs types [() :< VoidExpr])

i_expr (meta :< Call fn _ types args) = do
  (fn', tyFn) <- i_expr fn
  let tyFn' = normalizeFnType tyFn
  (tyFn''@(Fun gen _ retType), skippedVars) <- adjustFnType (null types) args tyFn'
  (retType', args', typeArgs)  <-
        case (tyFn'', types) of
          (Fun (_:_) params _, []) -> do
            (_, argsTy) <- unzip <$> mapM i_expr args
            (retType, typeArgs) <- inferTyArgs argsTy tyFn''
            let s = zipSubst (map fst gen) typeArgs
            let params' = map (applySubst s) params
            args' <- zipWithM instSubtype args params'
            return (retType, args', typeArgs)
          (Fun gen params _, _) -> do
            types' <- mapM resolveType types
            let s = zipSubst (map fst gen) types'
            let params' = map (applySubst s) params
            args' <- zipWithM instSubtype args params'
            return (applySubst s retType, args', types')
          _ -> undefined
  constraintArgs <- inferConstraintArgs gen skippedVars typeArgs
  return (meta :< Call fn' constraintArgs types args', retType')

i_expr (meta :< Record fields) = do
  (exprs, types) <- mapM (i_expr . snd) fields >>= return . unzip
  let labels = map fst fields
  let fieldsTy = zip labels types
  let recordTy = Rec fieldsTy
  let record = Record (zip labels exprs)
  return (meta :< record, recordTy)

i_expr (meta :< FieldAccess expr field) = do
  (expr', ty) <- i_expr expr
  let
      aux :: Type -> [(String, Type)] -> Tc (T.Expr, Type)
      aux ty r = case lookup field r of
                Nothing -> throwError $ UnknownField ty field
                Just t -> return (meta :< FieldAccess expr' field, t)
  case ty of
    Rec r -> aux ty r
    Cls _ -> do
      vars <- lookupInstanceVars ty
      aux ty vars
    _ -> throwError . GenericError $ "Expected a record, but found value of type " ++ show ty

i_expr (meta :< If ifCond ifBody elseBody) = do
  (ifCond', ty) <- i_expr ifCond
  ty <:! bool
  (ifBody', ifTy) <- i_body ifBody
  (elseBody', elseTy) <- i_body elseBody
  return (meta :< If ifCond' ifBody' elseBody', ifTy \/ elseTy)

i_expr (meta :< List _ items) = do
  (items', itemsTy) <- unzip <$> mapM i_expr items
  (ty, itemTy) <- case itemsTy of
                    [] -> do
                      nilTy <- lookupValue "Nil"
                      return (nilTy, Bot)
                    x:xs ->
                      let ty = foldl (\/) x xs
                       in return (list ty, ty)
  return (meta :< List (Just itemTy) items', ty)

i_expr (meta :< FnExpr fn) = do
  (fn', ty) <- i_fn fn
  return $ (meta :< FnExpr fn', ty)

i_expr (meta :< Negate _ expr) = do
  (expr', ty) <- i_expr expr
  intf <- lookupInterface "Std.Number"
  constrArgs <- boundsCheck ty intf
  return (meta :< Negate constrArgs expr', ty)

-- Expressions generated during type checking
i_expr (meta :< VoidExpr) = return (meta :< VoidExpr, void)

i_expr (_ :< TypeCall {}) = undefined

instSubtype :: U.Expr -> Type -> Tc T.Expr
instSubtype arg@(meta :< _) ty = do
  (arg', argTy) <- i_expr arg
  arg'' <- case (argTy, ty) of
             (Fun gen@(_:_) _ _, Fun [] _ _) -> do
               typeArgs <- inferTyAbs argTy ty
               constraintArgs <- inferConstraintArgs gen [] typeArgs
               return $ meta :< TypeCall arg' constraintArgs
             _ -> do
               argTy <:! ty
               return arg'
  return arg''

inferConstraintArgs :: [BoundVar] -> [BoundVar] -> [Type] -> Tc [ConstraintArg]
inferConstraintArgs gen skippedVars typeArgs = do
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
        concat <$> mapM (boundsCheck tyArg) bounds

-- Checks if `t1` implements the interface in `t2`
-- returns a list with a single ConstraintArg indicating
--  how to satisfy the bounds. See `ConstraintArg` for
--  an explanation on the kinds of ConstraintArgs
boundsCheck :: Type -> Intf -> Tc [ConstraintArg]
boundsCheck t1 t2@(Intf name _ _) = do
  args <- boundsCheck' t1 t2
  if null args
     then throwError $ MissingImplementation name t1
     else return args

boundsCheck' :: Type -> Intf -> Tc [ConstraintArg]
boundsCheck' v@(Var _ bounds) intf = do
  return $ if intf `elem` bounds
              then [CABound v intf]
              else []

boundsCheck' (TyApp ty args) intf@(Intf name _ _) = do
  implementations <- lookupImplementations name
  case lookup ty implementations of
    Nothing -> return []
    Just vars -> do
      let aux arg (_, bounds) =
            concat <$> mapM (boundsCheck arg) bounds
      args <- concat <$> zipWithM aux args vars
      return [CAPoly ty intf args]

boundsCheck' (Forall params ty) intf =
  boundsCheck' (params \\ ty) intf

boundsCheck' (TyAbs params ty) intf =
  boundsCheck' (params \\ ty) intf

boundsCheck' Bot intf =
  return [CABound Bot intf]

boundsCheck' ty intf@(Intf name _ _) = do
  implementations <- lookupImplementations name
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

i_case :: Type -> U.Case -> Tc (T.Case, Type)
i_case ty (meta :< Case pattern caseBody) = do
  m <- startMarker
  pattern' <- c_pattern ty pattern
  endMarker m

  (caseBody', ty) <- i_body caseBody

  clearMarker m

  return (meta :< Case pattern' caseBody', ty)

c_pattern :: Type -> U.Pattern -> Tc T.Pattern
c_pattern _ (meta :< PatDefault) =
  return $ meta :< PatDefault

c_pattern ty (meta :< PatLiteral l) = do
  let litTy = i_lit l
  litTy <:! ty
  return $ meta :< PatLiteral l

c_pattern ty (meta :< PatVar v) = do
  insertValue v ty
  return $ meta :< PatVar v

c_pattern ty@(Rec tyFields) (meta :< PatRecord fields) = do
  fields' <- mapM aux fields
  return $ meta :< PatRecord fields'
    where
      aux (key, pat) = do
        case lookup key tyFields of
          Just ty -> do
            pat' <- c_pattern ty pat
            return (key, pat')
          Nothing ->
            throwError . GenericError $ "Matching against field `" ++ key ++ "`, which is not included in the type of the value being matched, `" ++ show ty ++ "`"

c_pattern ty (_ :< PatRecord _) = do
  throwError . GenericError $ "Using a record pattern, but value being matched has type `" ++ show ty ++ "`"

c_pattern ty (meta :< PatList pats rest) = do
  itemTy <- getItemTy ty
  pats' <- mapM (c_pattern itemTy) pats
  rest' <- case rest of
             NoRest -> return NoRest
             DiscardRest -> return DiscardRest
             NamedRest n -> do
               insertValue n ty
               return (NamedRest n)
  return $ meta :< PatList pats' rest'
  where
    getItemTy (Forall _ (TyApp (Con "List") _)) =
      return Top

    getItemTy (TyApp (Con "List") [ty]) =
      return ty

    getItemTy _ =
      throwError . GenericError $ "Using a list pattern, but value being matched has type `" ++ show ty ++ "`"

c_pattern ty (meta :< PatCtor name vars) = do
  ctorTy <- lookupValue name
  let (fnTy, params, retTy) = case ctorTy of
                            fn@(Fun [] params retTy) -> (fn, params, retTy)
                            fn@(Fun gen params retTy) -> (fn, params, Forall (map fst gen) retTy)
                            t -> (Fun [] [] t, [], t)
  when (length vars /= length params) (throwError ArityMismatch)
  retTy <:! ty
  let substs = case (retTy, ty) of
                 (Forall gen _, TyApp _ args) -> zipSubst gen args
                 _ -> emptySubst
  let params' = map (applySubst substs) params
  vars' <- zipWithM c_pattern params' vars
  return $ meta :< PatCtor (name, fnTy) vars'
