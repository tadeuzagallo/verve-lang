module Typing.Decl (c_decl) where

import Typing.Ctx
import Typing.Expr
import Typing.Util
import Typing.State
import Typing.Substitution
import Typing.TypeError
import Typing.Types

import Absyn.Base
import Absyn.Meta
import Absyn.ValueOccursCheck
import qualified Absyn.Untyped as U
import qualified Absyn.Typed as T

import Control.Monad (foldM, when)
import Data.Foldable (foldrM)
import Data.List (intersect)

{-
   Δ : types context
   Γ : values context
   Ψ : implementations context (for brevity only mentioned on the necessary rules)
-}

c_decl :: Ctx -> U.Decl -> Tc (Ctx, T.Decl)

{-
  Δ; Γ ⊢ f : T
  ----------------------- FnDecl
  Δ; Γ ⊢ fn ⊣ Γ, f : T; Δ
-}
c_decl ctx (FnStmt fn) = do
  (fn', ty) <- i_fn ctx fn
  return (addValueType ctx (name fn, ty), FnStmt fn')

{-
   ∀ Ci ∈ C1..Cn, ∀ Tij ∈ Ti1..Tik, Δ, S1 : *, ..., Sn : *, E : arity(m, *); Γ ⊢ Tik : *
   ------------------------------------------------------------------------------------------------------------------ EnumDecl
   Δ; Γ ⊢ enum E<S1, ..., Sm> { C1(T11..T1j), ..., Cn(Tn1..Tnk) }
        ⊣ Δ, E: arity(m, *); Γ, C1 : ∀ S1, ..., Sm. T1 -> ... Tj -> E, ..., Cn : ∀ S1, ..., Sm. T1 -> ... -> Tk -> E
  -}
c_decl ctx (Enum name generics ctors) = do
  (ctx', generics') <- addGenerics ctx (defaultBounds generics)
  let mkEnumTy ty = case (ty, generics') of
                (Nothing, []) -> Con name
                (Nothing, _)  -> Forall (map fst generics') (TyApp (Con name) (map (uncurry Var) generics'))
                (Just t, [])   -> Fun generics' t (Con name)
                (Just t, _)   -> Fun generics' t (TyApp (Con name) (map (uncurry Var) generics'))
  let enumTy =
        if null generics
           then Con name
           else TyAbs (map fst generics') (TyApp (Con name) (map (uncurry Var) generics'))
  let ctx'' = addType ctx' (name, enumTy)
  (ctx''', ctors') <- foldrM (i_ctor ctx'' mkEnumTy) (ctx, []) ctors
  return (addType ctx''' (name, enumTy), (Enum (name, enumTy) generics ctors'))

{-
                   S :: *              T :: *              U :: *
                   Δ, S1 : *, ..., Sn : *; Γ, x : S, y: U ⊢ e : V
                                   V <: U
   -----------------------------------------------------------------------------------
   Δ; Γ ⊢ operator<V1, ..., Vn> (x : S) OP (y : T) -> U { e } ⊣ Δ; Γ, OP : S -> T -> U
-}
c_decl ctx (Operator opAssoc opPrec opGenerics opLhs opName opRhs opRetType opBody) = do
  opGenerics' <- resolveGenerics ctx opGenerics
  (ctx', opGenericVars) <- addGenerics ctx opGenerics'
  opLhs' <- resolveId ctx' opLhs
  opRhs' <- resolveId ctx' opRhs
  opRetType' <- resolveType ctx' opRetType
  let ctx'' = addValueType (addValueType ctx' opLhs') opRhs'
  (opBody', bodyTy) <- i_body ctx'' opBody
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
  return (addValueType ctx (opName, ty), op')

{-

   T :: *      Δ; Γ, x: T ⊢ e : S     S <: T
   ----------------------------------------- LetRecDecl
      Δ; Γ ⊢ let x : T = e ⊣ Δ; Γ, x : T

           Δ; Γ ⊢ e : S
   ------------------------------ LetDecl
   Δ; Γ ⊢ let x = e ⊣ Δ; Γ, x : S
-}
c_decl ctx (Let (var, ty) expr) = do
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
  return (ctx', let')

c_decl ctx cls@(Class {}) = do
  i_class ctx cls

{-
   NOTE: interface lets must bind to functions for now - it's get a bit more complicated with arbitrary types

   ∀ i ∈ 1..n, Δ, T :: * ⊢ Ti <: ⊥ -> ⊤
   ----------------------------------------------------- InterfaceDecl
   Δ; Γ ⊢ interface  I<T> {
     decl1 : T1
     ...
     decln : Tn
  } ⊣ Δ, I : Interface { decl1 : T1, ... decln : Tn } ;
      Γ, decl1 : ∀(T: I). T1, ..., decln :  ∀(T : I). Tn
-}
c_decl ctx (Interface name param methods) = do
  (ctx', [(param', [])]) <- addGenerics ctx [(param, [])]
  (methods', methodsTy) <- unzip <$> mapM (i_interfaceItem ctx') methods
  let ty = Intf name param' methodsTy
  let intf = Interface (name, void) param methods'
  ctx' <- foldM (aux ty param') ctx methodsTy
  return (addInterface ctx' (name, ty), intf)
    where
      aux intf param ctx (name, Fun gen params retType) =
        let ty = Fun ((param, [intf]) : gen) params retType
         in return $ addValueType ctx (name, ty)
      aux _ _ _ _ = throwError $ GenericError "let bindings within interfaces must be functions"

{-
   TODO: Check for existing implementation for type

  ∀ i ∈ 1..n, Δ, U1 :: *, ..., Un :: *; Γ ⊢ impli : T /\ T <: Δ(I)[impli]
  ---------------------------------------------- ImplementationDecl
   Δ; Γ; Ψ ⊢ implement<U1, ..., Un> I<T> {
     impl1
     ...
     impln
   } ⊣ Δ; Γ; Ψ, I<T> : { impl1, ..., impln }
-}
c_decl ctx (Implementation implName generics ty methods) = do
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
  return (ctx', impl)
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

{-
  Δ, T1 :: *, ..., Tn :: *; Γ ⊢ well_formed U
  ------------------------------------------------- AliasTypeDecl
  Δ; Γ ⊢ type S<T1, ..., Tn> = U ⊣ Δ, ΛT1..Tn. U; Γ
-}
c_decl ctx (TypeAlias aliasName aliasVars aliasType) = do
  (ctx', aliasVars') <- addGenerics ctx (defaultBounds aliasVars)
  aliasType' <- resolveType ctx' aliasType
  let aliasType'' = case aliasVars' of
                [] -> aliasType'
                _  -> TyAbs (map fst aliasVars') aliasType'
  let alias = TypeAlias aliasName aliasVars aliasType''
  return (addType ctx (aliasName, aliasType''), alias)


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
  (body', bodyTy) <- i_body ctxWithParams body
  bodyTy <:! retTy
  return (ImplFunction (name, intfTy) params body', name)

i_implItem ctx subst intfTypes (ImplOperator lhs op rhs body) = do
  intfTy <- applySubst subst <$> getIntfType op intfTypes
  Fun _ paramsTy retTy <- case intfTy of
                            Fun _ ps _ | length ps == 2 -> return intfTy
                            _ -> throwError $ GenericError "Implementation type doesn't match interface"
  let ctxWithParams = foldl addValueType ctx (zip [lhs, rhs] paramsTy)
  (body', bodyTy) <- i_body ctxWithParams body
  bodyTy <:! retTy
  return (ImplOperator lhs (op, intfTy) rhs body', op)


i_ctor :: Ctx -> (Maybe [Type] -> Type) -> U.DataCtor -> (Ctx, [T.DataCtor]) -> Tc (Ctx, [T.DataCtor])
i_ctor sourceCtx mkEnumTy (name, types) (targetCtx, ctors) = do
  types' <- sequence (mapM (resolveType sourceCtx) <$> types)
  let ty = mkEnumTy types'
  return (addValueType targetCtx (name, ty), ((name, ty), types'):ctors)

i_class :: Ctx -> U.Decl -> Tc (Ctx, T.Decl)
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
  return (ctxWithMethods, class')

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
