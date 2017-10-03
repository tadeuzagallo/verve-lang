{-# LANGUAGE NamedFieldPuns #-}

module Typing.TypeChecker
  ( infer
  , inferStmt
  ) where

import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

import Absyn.Base
import Absyn.Meta
import Absyn.ValueOccursCheck
import Error
import Typing.Constraint
import Typing.Ctx
import Typing.Kinds
import Typing.State
import Typing.Substitution
import Typing.Subtyping
import Typing.TypeError
import Typing.Types

import Control.Monad (foldM, when, zipWithM)
import Data.Bifunctor (first)
import Data.Foldable (foldrM)
import Data.List (intersect, union)

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

infer :: U.Module -> Result (T.Module, Type)
infer (Module imports stmts) =
  runTc
    (i_stmts defaultCtx stmts)
    (\(stmts, ty) -> (Module imports stmts, ty))

inferStmt :: Ctx -> U.Stmt -> Result (Ctx, T.Stmt, Type)
inferStmt ctx stmt =
  runTc (i_stmt ctx stmt) id

i_stmts :: Ctx -> [U.Stmt] -> Tc ([T.Stmt], Type)
i_stmts ctx stmts = do
  (_, stmts', ty) <- foldM aux (ctx, [], void) stmts
  return (reverse stmts', ty)
    where
      aux :: (Ctx, [T.Stmt], Type) -> U.Stmt -> Tc (Ctx, [T.Stmt], Type)
      aux (ctx, stmts, _) stmt = do
        (ctx', stmt', ty) <- i_stmt ctx stmt
        return (ctx', stmt':stmts, ty)

i_stmt :: Ctx -> U.Stmt -> Tc (Ctx, T.Stmt, Type)
i_stmt ctx (Expr expr) = do
  (expr', ty) <- i_expr ctx expr
  return (ctx, Expr expr', ty)
i_stmt ctx (Decl decl) = do
  (ctx', decl', ty) <- i_decl ctx decl
  return (ctx', Decl decl', ty)

i_decl :: Ctx -> U.Decl -> Tc (Ctx, T.Decl, Type)
i_decl ctx (FnStmt fn) = do
  (fn', ty) <- i_fn ctx fn
  return (addValueType ctx (name fn, ty), FnStmt fn', ty)

i_decl ctx (Enum name generics ctors) = do
  (ctx', generics') <- addGenerics ctx (defaultBounds generics)
  let mkEnumTy ty = case (ty, generics') of
                (Nothing, []) -> Con name
                (Just t, [])  -> Fun [] t (Con name)
                (Nothing, _)  -> TyAbs (map fst generics') (TyApp (Con name) (map (uncurry Var) generics'))
                (Just t, _)   -> Fun generics' t (TyApp (Con name) (map (uncurry Var) generics'))
  let enumTy = mkEnumTy Nothing
  let ctx'' = addType ctx' (name, enumTy)
  (ctx''', ctors') <- foldrM (i_ctor ctx'' mkEnumTy) (ctx, []) ctors
  return (addType ctx''' (name, enumTy), (Enum (name, enumTy) generics ctors'), Type)

i_decl ctx (Operator opAssoc opPrec opGenerics opLhs opName opRhs opRetType opBody) = do
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

i_decl ctx (Let (var, ty) expr) = do
  (ctx', expr', exprTy) <-
    case ty of
      U.TPlaceholder -> do
        (expr', exprTy) <- i_expr ctx expr
        let ctx' = addValueType ctx (var, exprTy)
        return (ctx', expr', exprTy)
      _ -> do
        ty' <- resolveType ctx ty
        let ctx' = addValueType ctx (var, ty')
        _ <- valueOccursCheck var expr
        (expr', exprTy) <- i_expr ctx' expr
        exprTy <:! ty'
        return (ctx',expr', ty')
  let let' = Let (var, exprTy) expr'
  return (ctx', let', exprTy)

i_decl ctx cls@(Class {}) = do
  i_class ctx cls

i_decl ctx (Interface name param methods) = do
  (ctx', [(param', [])]) <- addGenerics ctx [(param, [])]
  (methods', methodsTy) <- unzip <$> mapM (i_interfaceItem ctx') methods
  let ty = Intf name param' methodsTy
  let intf = Interface (name, void) param methods'
  let ctx' = foldl (aux ty param') ctx methodsTy
  return (addInterface ctx' (name, ty), intf, void)
    where
      aux intf param ctx (name, Fun gen params retType) =
        let ty = Fun ((param, [intf]) : gen) params retType
         in addValueType ctx (name, ty)
      aux _ _ _ _ = undefined

i_decl ctx (Implementation implName generics ty methods) = do
  Intf _ param intfMethods <- getInterface implName ctx
  generics' <- resolveGenerics ctx generics
  (ctx', genericVars) <- addGenerics ctx generics'
  ty' <- resolveType ctx' ty
  ctx'' <- extendCtx ctx' ty' genericVars
  let substs = mkSubst (param, ty')
  (methods', names) <- unzip <$> mapM (i_implItem ctx'' substs intfMethods) methods
  checkCompleteInterface intfMethods names
  let impl = Implementation (implName, void) generics' ty' methods'
  ctx' <- extendCtx ctx ty' genericVars
  return (ctx', impl, void)
  where
    extendCtx ctx (TyApp ty args) genericVars@(_:_) | vars  `intersect` args == vars =
      addImplementation ctx (implName, (ty, genericVars))
        where
          vars = map (uncurry Var) genericVars
    extendCtx _ ty@(TyApp _ _) [] =
      throwError (ImplementationError implName ty)
    extendCtx ctx ty [] =
      addImplementation ctx (implName, (ty, []))
    extendCtx _ ty _ =
      throwError (ImplementationError implName ty)

i_decl ctx (TypeAlias aliasName aliasVars aliasType) = do
  (ctx', aliasVars') <- addGenerics ctx (defaultBounds aliasVars)
  aliasType' <- resolveType ctx' aliasType
  let aliasType'' = case aliasVars' of
                [] -> aliasType'
                _  -> TyAbs (map fst aliasVars') aliasType'
  let alias = TypeAlias aliasName aliasVars aliasType''
  return (addType ctx (aliasName, aliasType''), alias, Type)


checkCompleteInterface :: [(Name, Type)] -> [Name] -> Tc ()
checkCompleteInterface intf impl = do
  mapM_ aux intf
  where
    aux :: (Name, Type) -> Tc ()
    aux (methodName, _) =
      when (methodName `notElem` impl) $ throwError (ImplementationMissingMethod methodName)


getIntfType :: Name -> [(Name, Type)] -> Tc Type
getIntfType name intf = do
  case lookup name intf of
    Nothing -> throwError $ ExtraneousImplementation name
    Just ty -> return ty


-- TODO: this should switch to checking mode
i_implItem :: Ctx -> Substitution -> [(Name, Type)] -> U.ImplementationItem -> Tc (T.ImplementationItem, Name)
i_implItem ctx subst intfTypes (ImplVar (name, expr)) = do
  (expr', exprTy) <- i_expr ctx expr
  intfTy <- applySubst subst <$> getIntfType name intfTypes
  intfTy <:! exprTy
  return (ImplVar ((name, intfTy), expr'), name)

i_implItem ctx subst intfTypes (ImplFunction name params body) = do
  intfTy <- applySubst subst <$> getIntfType name intfTypes
  Fun _ paramsTy retTy <- case intfTy of
                            Fun _ ps _ | length ps == length params -> return intfTy
                            _ -> throwError $ GenericError "Implementation type doesn't match interface"
  let ctxWithParams = foldl addValueType ctx (zip params paramsTy)
  (body', bodyTy) <- i_stmts ctxWithParams body
  bodyTy <:! retTy
  return (ImplFunction (name, intfTy) params body', name)

i_implItem ctx subst intfTypes (ImplOperator lhs op rhs body) = do
  intfTy <- applySubst subst <$> getIntfType op intfTypes
  Fun _ paramsTy retTy <- case intfTy of
                            Fun _ ps _ | length ps == 2 -> return intfTy
                            _ -> throwError $ GenericError "Implementation type doesn't match interface"
  let ctxWithParams = foldl addValueType ctx (zip [lhs, rhs] paramsTy)
  (body', bodyTy) <- i_stmts ctxWithParams body
  bodyTy <:! retTy
  return (ImplOperator lhs (op, intfTy) rhs body', op)


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

i_ctor :: Ctx -> (Maybe [Type] -> Type) -> U.DataCtor -> (Ctx, [T.DataCtor]) -> Tc (Ctx, [T.DataCtor])
i_ctor sourceCtx mkEnumTy (name, types) (targetCtx, ctors) = do
  types' <- sequence (types >>= return . mapM (resolveType sourceCtx))
  let ty = mkEnumTy types'
  return (addValueType targetCtx (name, ty), ((name, ty), types'):ctors)

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
  (body', bodyTy) <- i_stmts ctx''' (body fn)
  bodyTy <:! retType'
  let fn' = fn { name = (name fn, ty)
               , generics = gen'
               , params = tyArgs
               , retType = retType'
               , body = body'
               }
  return (fn', ty)

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
  (ifBody', ifTy) <- i_stmts ctx ifBody
  (elseBody', elseTy) <- i_stmts ctx elseBody
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
  (caseBody', ty) <- i_stmts ctx' caseBody
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
    getItemTy (TyAbs vars ty) =
      return $ vars // ty

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
  return (PatCtor (name, fnTy) vars', ctx')
    where
      aux (vars, ctx) (ty, var) = do
        (var', ctx') <- c_pattern ctx ty var
        return (var':vars, ctx')

i_class :: Ctx -> U.Decl -> Tc (Ctx, T.Decl, Type)
i_class ctx (Class name vars methods) = do
  let classTy = Cls name
  let ctxWithClass = addType ctx (name, classTy)
  vars' <- mapM (resolveId ctxWithClass) vars
  let ctorTy = [Rec vars'] ~> classTy
  let ctxWithCtor = addValueType ctxWithClass (name, ctorTy)
  let ctxWithVars = addInstanceVars ctxWithCtor (classTy, vars')
  ctxWithMethods <- foldM (i_addMethodType classTy) ctxWithVars methods
  methods' <- mapM (i_method classTy ctxWithMethods) methods
  let class' = Class (name, classTy) vars' methods'
  return (ctxWithMethods, class', Type)

i_class _ _ = undefined

i_addMethodType :: Type -> Ctx -> U.Function -> Tc Ctx
i_addMethodType classTy ctx fn = do
  let ctxWithSelf = addType ctx ("Self", classTy)
  let fn' = fn { params = ("self", U.TName "Self") : params fn }
  gen' <- resolveGenerics ctxWithSelf $ generics fn'
  (ctxWithGenerics, genericVars) <- addGenerics ctxWithSelf gen'
  (ty, _, _) <- fnTy ctxWithGenerics (genericVars, params fn', retType fn')
  return $ addValueType ctx (name fn', ty)

i_method :: Type -> Ctx -> U.Function -> Tc T.Function
i_method classTy ctx fn = do
  let ctx' = addType ctx ("Self", classTy)
  let fn' = fn { params = ("self", U.TName "Self") : params fn }
  (fn'', _) <- i_fn ctx' fn'
  return fn''


i_interfaceItem :: Ctx -> U.InterfaceItem -> Tc (T.InterfaceItem, T.Id)
i_interfaceItem ctx (IntfVar id) = do
  id' <- resolveId ctx id
  return (IntfVar id', id')

i_interfaceItem ctx op@(IntfOperator _ _ lhs name rhs retType) = do
  lhs' <- resolveType ctx lhs
  rhs' <- resolveType ctx rhs
  retType' <- resolveType ctx retType
  let ty = Fun [] [lhs', rhs'] retType'
  let op' = op { intfOpLhs = lhs'
               , intfOpName = (name, ty)
               , intfOpRhs = rhs'
               , intfOpRetType = retType'
               }
  return (op', (name, ty))
