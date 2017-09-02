{-# LANGUAGE NamedFieldPuns #-}

module Typing.TypeChecker
  ( infer
  , inferStmt
  ) where

import Absyn
import Error
import Typing.Constraint
import Typing.Ctx
import Typing.State
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types

import Control.Monad (foldM, when, zipWithM, zipWithM_)
import Data.Bifunctor (first)
import Data.Foldable (foldrM)
import Data.List (union)

(<:!) :: Type -> Type -> Tc ()
actualTy <:! expectedTy =
  when (not $ actualTy <: expectedTy) (throwError $ TypeError expectedTy actualTy)

resolveId :: Ctx -> Id UnresolvedType -> Tc (Id Type)
resolveId ctx (n, ty) = (,) n  <$> resolveType ctx ty

resolveType :: Ctx -> UnresolvedType -> Tc Type
resolveType ctx (UTName v) =
  getType v ctx
resolveType ctx (UTArrow params ret) = do
  params' <- mapM (resolveType ctx) params
  ret' <- resolveType ctx ret
  return $ Fun [] params' ret'
resolveType ctx (UTRecord fieldsTy) = do
  fieldsTy' <- mapM (resolveId ctx) fieldsTy
  return $ Rec fieldsTy'
resolveType ctx (UTApp t1 t2) = do
  t1' <- resolveType ctx t1
  t2' <- mapM (resolveType ctx) t2
  case t1' of
    -- TODO: this doesn't seem right
    TyAbs params ty ->
      return $ applySubst (zipSubst params t2') ty
    _ -> return $ TyApp t1' t2'
resolveType _ UTVoid = return void
resolveType _ UTPlaceholder = undefined

resolveGenerics :: Ctx -> [(Name, [UnresolvedType])] -> Tc [(Name, [Type])]
resolveGenerics ctx gen =
  mapM resolve gen
    where
      resolve (name, bounds) = do
        bounds' <- mapM resolveConstraint bounds
        return (name, bounds')

      resolveConstraint ty = do
        ty' <- resolveType ctx ty
        case ty' of
          Intf _ _  _ -> return ty'
          _ -> throwError $ InterfaceExpected ty'

addGenerics :: Ctx -> [(Name, [Type])] -> Tc (Ctx, [(Var, [Type])])
addGenerics ctx generics =
  foldM aux (ctx, []) generics
    where
      aux (ctx, vars) (g, bounds) = do
        g' <- newVar g
        return (addType ctx (g, Var g' bounds), vars ++ [(g', bounds)])

defaultBounds :: [a] -> [(a, [b])]
defaultBounds = map (flip (,) [])

infer :: Module Name UnresolvedType -> Result (Module (Id Type) Type, Type)
infer mod =
  runTc
    (i_stmts defaultCtx (stmts mod))
    (\(stmts, ty) -> (Module stmts, ty))

inferStmt :: Ctx -> Stmt Name UnresolvedType -> Result (Ctx, Stmt (Id Type) Type, Type)
inferStmt ctx stmt =
  runTc (i_stmt ctx stmt) id

i_stmts :: Ctx -> [Stmt Name UnresolvedType ] -> Tc ([Stmt (Id Type) Type], Type)
i_stmts ctx stmts = do
  (_, stmts', ty) <- foldM aux (ctx, [], void) stmts
  return (reverse stmts', ty)
    where
      aux :: (Ctx, [Stmt (Id Type) Type], Type) -> Stmt Name  UnresolvedType -> Tc (Ctx, [Stmt (Id Type) Type], Type)
      aux (ctx, stmts, _) stmt = do
        (ctx', stmt', ty) <- i_stmt ctx stmt
        return (ctx', stmt':stmts, ty)

i_stmt :: Ctx -> Stmt Name UnresolvedType -> Tc (Ctx, Stmt (Id Type) Type, Type)
i_stmt ctx (Expr expr) = do
  (expr', ty) <- i_expr ctx expr
  return (ctx, Expr expr', ty)

i_stmt ctx (FnStmt fn) = do
  (fn', ty) <- i_fn ctx fn
  return (addValueType ctx (name fn, ty), FnStmt fn', ty)

i_stmt ctx (Enum name generics ctors) = do
  (ctx', generics') <- addGenerics ctx (defaultBounds generics)
  let mkEnumTy ty = case (ty, generics') of
                (Nothing, []) -> Con name
                (Just t, [])  -> Fun [] t (Con name)
                (Nothing, _)  -> TyAbs (map fst generics') (TyApp (Con name) (map (uncurry Var) generics'))
                (Just t, _)   -> Fun generics' t (TyApp (Con name) (map (uncurry Var) generics'))
  let enumTy = mkEnumTy Nothing
  let ctx'' = addType ctx' (name, enumTy)
  (ctx''', ctors') <- foldrM (i_ctor mkEnumTy) (ctx'', []) ctors
  return (ctx''', (Enum (name, enumTy) generics ctors'), Type)

i_stmt ctx (Operator opAssoc opPrec opGenerics opLhs opName opRhs opRetType opBody) = do
  opGenerics' <- resolveGenerics ctx opGenerics
  (ctx', opGenericVars) <- addGenerics ctx opGenerics'
  opLhs' <- resolveId ctx' opLhs
  opRhs' <- resolveId ctx' opRhs
  opRetType' <- resolveType ctx' opRetType
  let ctx'' = addValueType (addValueType ctx' opLhs') opRhs'
  (opBody', bodyTy) <- i_stmts ctx'' opBody
  bodyTy <:! opRetType'
  let ty = Fun opGenericVars [snd opLhs', snd opRhs'] opRetType'
  let op' = Operator { opAssoc
                     , opPrec
                     , opGenerics = opGenerics'
                     , opLhs = opLhs'
                     , opName = (opName, ty)
                     , opRhs = opRhs'
                     , opRetType = opRetType'
                     , opBody = opBody' }
  return (addValueType ctx (opName, ty), op', ty)

i_stmt ctx (Let var expr) = do
  (expr', exprTy) <- i_expr ctx expr
  let ctx' = addValueType ctx (var, exprTy)
  let let' = Let (var, exprTy) expr'
  return (ctx', let', exprTy)

i_stmt ctx (Class name vars methods) = do
  vars' <- mapM (resolveId ctx) vars
  let classTy = Cls name vars'
  let ctorTy = [Rec vars'] ~> classTy
  let ctx' = addType ctx (name, classTy)
  let ctx'' = addValueType ctx' (name, ctorTy)

  (ctx''', methods') <- foldM (i_method classTy) (ctx'', []) methods
  let class' = Class (name, classTy) vars' methods'
  return (ctx''', class', Type)

i_stmt ctx (Interface name param methods) = do
  (ctx', [(param', [])]) <- addGenerics ctx [(param, [])]
  (methods', methodsTy) <- unzip <$> mapM (i_fnDecl ctx') methods
  let ty = Intf name param' methodsTy
  let intf = Interface (name, ty) param methods'
  let ctx' = foldl (aux ty param') ctx methodsTy
  return (addType ctx' (name, ty), intf, ty)
    where
      aux intf param ctx (name, Fun gen params retType) =
        let ty = Fun ((param, [intf]) : gen) params retType
         in addValueType ctx (name, ty)
      aux _ _ _ _ = undefined

i_stmt ctx (Implementation implName generics ty methods) = do
  Intf _ param intfMethods <- getType implName ctx
  -- TODO: proper error in intf is not an Intf
  generics' <- resolveGenerics ctx generics
  (ctx', genericVars) <- addGenerics ctx generics'
  ty' <- resolveType ctx' ty
  ctx'' <- extendCtx ctx' ty' genericVars
  (methods', methodsTy) <- unzip <$> mapM (i_fnNonRec ctx'') methods
  let substs = mkSubst (param, ty')
  checkCompleteInterface substs intfMethods (zip (map name methods) methodsTy)
  checkExtraneousMethods intfMethods (zip (map name methods) methodsTy)
  let impl = Implementation (implName, void) generics' ty' methods'
  ctx' <- extendCtx ctx ty' genericVars
  return (ctx', impl, void)
  where
    extendCtx ctx (TyApp ty _) genericVars@(_:_) =
      addInstance ctx (implName, (ty, genericVars))
    extendCtx ctx ty [] =
      addInstance ctx (implName, (ty, []))
    extendCtx _ _ _ =
      throwError ImplementationError

checkCompleteInterface :: Substitution -> [(Name, Type)] -> [(Name, Type)] -> Tc ()
checkCompleteInterface substs intf impl = do
  mapM_ aux intf
  where
    aux :: (Name, Type) -> Tc ()
    aux (methodName, methodTy) =
      case lookup methodName impl of
        Nothing -> throwError $ MissingImplementation methodName
        Just ty -> ty <:! (applySubst substs methodTy)

checkExtraneousMethods :: [(Name, Type)] -> [(Name, Type)] -> Tc ()
checkExtraneousMethods intf impl = do
  mapM_ aux impl
  where
    aux :: (Name, Type) -> Tc ()
    aux (methodName, _) =
      case lookup methodName intf of
        Nothing -> throwError $ ExtraneousImplementation methodName
        Just _ -> return ()

i_fnDecl :: Ctx -> FunctionDecl Name UnresolvedType -> Tc (FunctionDecl (Id Type) Type, (Name, Type))
i_fnDecl ctx (FunctionDecl name gen params retType) = do
  gen' <- resolveGenerics ctx gen
  (ctx', genVars) <- addGenerics ctx gen'
  (ty, params', retType') <- fnTy ctx' (genVars, params, retType)
  let fnDecl = FunctionDecl (name, ty) gen' params' retType'
  return (fnDecl, (name, ty))

fnTy :: Ctx -> ([(Var, [Type])], [(Name, UnresolvedType)], UnresolvedType) -> Tc (Type, [(Name, Type)], Type)
fnTy ctx (generics, params, retType) = do
  tyArgs <- mapM (resolveId ctx) params
  retType' <- resolveType ctx retType
  let tyArgs' = if null tyArgs
      then [void]
      else map snd tyArgs
  let ty = Fun generics tyArgs' retType'
  return (ty, tyArgs, retType')

i_method :: Type -> (Ctx, [Function (Id Type) Type]) -> Function Name UnresolvedType -> Tc (Ctx, [Function (Id Type) Type])
i_method classTy (ctx, fns) fn = do
  let ctx' = addType ctx ("Self", classTy)
  let fn' = fn { params = ("self", UTName "Self") : params fn }
  (fn'', fnTy) <- i_fn ctx' fn'
  return (addValueType ctx (name fn, fnTy), fn'' : fns)

i_ctor :: (Maybe [Type] -> Type) -> DataCtor Name UnresolvedType -> (Ctx, [DataCtor (Id Type) Type]) -> Tc (Ctx, [DataCtor (Id Type) Type])
i_ctor mkEnumTy (name, types) (ctx, ctors) = do
  types' <- sequence (types >>= return . mapM (resolveType ctx))
  let ty = mkEnumTy types'
  return (addValueType ctx (name, ty), ((name, ty), types'):ctors)

i_fn :: Ctx -> Function Name UnresolvedType -> Tc (Function (Id Type) Type, Type)
i_fn = i_fnBase True

i_fnNonRec :: Ctx -> Function Name UnresolvedType -> Tc (Function (Id Type) Type, Type)
i_fnNonRec = i_fnBase False

i_fnBase :: Bool -> Ctx -> Function Name UnresolvedType -> Tc (Function (Id Type) Type, Type)
i_fnBase addToCtx ctx fn = do
  gen' <- resolveGenerics ctx $ generics fn
  (ctx', genericVars) <- addGenerics ctx gen'
  (ty, tyArgs, retType') <- fnTy ctx' (genericVars, params fn, retType fn)
  let ctx'' = if addToCtx
                then addValueType ctx' (name fn, ty)
                else ctx'
  let ctx''' = foldl addValueType ctx'' tyArgs
  (body', bodyTy) <- i_stmts ctx''' (body fn)
  bodyTy <:! retType'
  let fn' = fn { name = (name fn, ty)
               , generics = gen'
               , params = tyArgs
               , retType = retType'
               , body = body'
               }
  return (fn', ty)

i_expr :: Ctx -> Expr Name UnresolvedType -> Tc (Expr (Id Type) Type, Type)
i_expr _ (Literal lit) = return (Literal lit, i_lit lit)

i_expr ctx (Ident i) = do
  ty <- getValueType i ctx
  return (Ident (i, ty), ty)

i_expr _ VoidExpr = return (VoidExpr, void)

i_expr ctx (ParenthesizedExpr expr) = i_expr ctx expr

i_expr ctx (BinOp _ _ lhs op rhs) = do
  tyOp@(Fun gen _ _) <- getValueType op ctx
  (lhs', lhsTy) <- i_expr ctx lhs
  (rhs', rhsTy) <- i_expr ctx rhs
  (retType', typeArgs)  <- inferTyArgs [lhsTy, rhsTy] tyOp
  (typeArgs', constraintArgs) <- adjustTypeArgs ctx gen [] typeArgs
  return (BinOp constraintArgs typeArgs' lhs' (op, tyOp) rhs', retType')

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
  (args', tyArgs) <- mapM (i_expr ctx) args >>= return . unzip
  types' <- mapM (resolveType ctx) types
  let tyFn' = normalizeFnType tyFn
  (tyFn''@(Fun gen _ retType), skippedVars) <- adjustFnType (null types) tyArgs tyFn'
  (retType', typeArgs)  <-
        case (tyFn'', types') of
          (Fun (_:_) _ _, []) ->
            inferTyArgs tyArgs tyFn''
          (Fun gen params _, _) -> do
            let s = zipSubst (map fst gen) types'
            let params' = map (applySubst s) params
            zipWithM_ (<:!) tyArgs params'
            return (applySubst s retType, types')
          _ -> undefined
  (typeArgs', constraintArgs) <- adjustTypeArgs ctx gen skippedVars typeArgs
  return (Call fn' constraintArgs typeArgs' args', retType')

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
      aux :: Type -> [(String, Type)] -> Tc (Expr (Id Type) Type, Type)
      aux ty r = case lookup field r of
                Nothing -> throwError $ UnknownField ty field
                Just t -> return (FieldAccess expr' ty (field, t), t)
  case ty of
    Rec r -> aux ty r
    Cls _ r -> aux ty r
    _ -> throwError . GenericError $ "Expected a record, but found value of type " ++ show ty

i_expr ctx (If ifCond ifBody elseBody) = do
  (ifCond', ty) <- i_expr ctx ifCond
  ty <:! bool
  (ifBody', ifTy) <- i_stmts ctx ifBody
  (elseBody', elseTy) <- i_stmts ctx elseBody
  return (If ifCond' ifBody' elseBody', ifTy \/ elseTy)

i_expr ctx (List items) = do
  (items', itemsTy) <- unzip <$> mapM (i_expr ctx) items
  ty <- case itemsTy of
          [] -> getValueType "Nil" ctx
          x:xs -> return . list $ foldl (\/) x xs
  return (List items', ty)

i_expr ctx (FnExpr fn) =
  first FnExpr <$> i_fn ctx fn

adjustTypeArgs :: Ctx -> [(Var, [Type])] -> [(Var, [Type])] -> [Type] -> Tc ([Type], [(Type, Type)])
adjustTypeArgs ctx gen skippedVars typeArgs = do
  constrArgs <- concat <$> zipWithM findConstrArgs gen typeArgs'
  return ([], constrArgs)
    where
      typeArgs' = zipWith findHoles gen typeArgs

      findHoles var ty =
        if var `elem` skippedVars
           then mkHole var
           else ty

      findConstrArgs (_, []) tyArg = do
        return [(tyArg, Type)]

      findConstrArgs (_, bounds) tyArg = do
        concat <$> mapM (boundsCheck ctx tyArg) bounds

boundsCheck :: Ctx -> Type -> Type -> Tc [(Type, Type)]
boundsCheck ctx t1 t2@(Intf name _ _) = do
  args <- boundsCheck' ctx t1 t2
  if null args
     then throwError $ MissingInstance name t1
     else return args

boundsCheck _ _ ty =
  throwError $ InterfaceExpected ty

boundsCheck' :: Ctx -> Type -> Type -> Tc [(Type, Type)]
boundsCheck' _ v@(Var _ bounds) intf = do
  return $ if intf `elem` bounds
              then [(v, intf), (v, Type)]
              else []

boundsCheck' ctx (TyApp ty args) intf@(Intf name _ _) = do
  instances <- getInstances name ctx
  case lookup ty instances of
    Nothing -> return []
    Just vars ->
      ([(ty, intf), (ty, Type)] ++) <$> (concat <$> zipWithM (\arg (_, bounds) ->
        concat <$> mapM (boundsCheck ctx arg) bounds
           ) args vars)

boundsCheck' ctx ty intf@(Intf name _ _) = do
  instances <- getInstances name ctx
  case lookup ty instances of
    Just [] -> return [(ty, intf), (ty, Type)]
    _ -> return []

boundsCheck' ctx (TyAbs params ty) intf =
  boundsCheck' ctx (params \\ ty) intf

boundsCheck' _ _ _ =
  return []

normalizeFnType :: Type -> Type
normalizeFnType (Fun gen params (Fun [] params' retTy)) =
  normalizeFnType (Fun gen (params ++ params') retTy)
normalizeFnType ty = ty

adjustFnType :: Bool -> [a] -> Type -> Tc (Type, [(Var, [Type])])
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

i_case :: Ctx -> Type -> Case Name UnresolvedType -> Tc (Case (Id Type) Type, Type)
i_case ctx ty (Case pattern caseBody) = do
  (pattern', ctx') <- c_pattern ctx ty pattern
  (caseBody', ty) <- i_stmts ctx' caseBody
  return (Case pattern' caseBody', ty)

c_pattern :: Ctx -> Type -> Pattern Name -> Tc (Pattern (Id Type), Ctx)
c_pattern ctx _ PatDefault = return (PatDefault, ctx)
c_pattern ctx ty (PatLiteral l) = do
  let litTy = i_lit l
  litTy <:! ty
  return (PatLiteral l, ctx)
c_pattern ctx ty (PatVar v) =
  let pat = PatVar (v, ty)
      ctx' = addValueType ctx (v, ty)
   in return (pat, ctx')
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
  return (PatCtor (name, fnTy) vars', ctx')
    where
      aux (vars, ctx) (ty, var) = do
        (var', ctx') <- c_pattern ctx ty var
        return (var':vars, ctx')
