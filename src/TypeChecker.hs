module TypeChecker (type_check, Context, TcId(..)) where

import AST
import Type

import Control.Monad (when, liftM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (State, runState, get, gets, put, modify)
import qualified Data.Map as Map
import Data.Foldable (foldlM)
import Data.Maybe (isNothing, fromJust)
import Text.Printf (printf)

type Context = (Map.Map String TyType)
type Error = (SourcePos, String)
type TcState = ExceptT Error (State Context)

data TcId = TcId String TyType deriving (Show)
type TcRes t = TcState (t TcId, TyType)

type_check :: Program String -> (Either Error (Program TcId), Context)
type_check program =
  let (res, ctx) = runState (runExceptT $ typeof_program program) Map.empty
   in (liftM fst res, ctx)

typeof_program :: Program String -> TcRes Program
typeof_program (Program _ decls) = do
  decls' <- mapM typeof_decl decls
  return $ (Program [] (map fst decls'), TyVoid)

typeof_decl :: TopDecl String -> TcRes TopDecl
typeof_decl (InterfaceDecl interface) = do
  (i, ty) <- typeof_interface interface
  return (InterfaceDecl i, ty)

typeof_decl (ImplementationDecl implementation) = do
  (i, ty) <- typeof_implementation implementation
  return (ImplementationDecl i, ty)

typeof_decl (ExternDecl (Prototype name signature)) = do
  (proto, fn_type) <- typeof_fn_type signature
  modify $ Map.insert name fn_type
  return (ExternDecl (Prototype (TcId name fn_type) proto), fn_type)

{-typeof_decl (TypeDecl enum_type) =-}
  {-return (TyVoid-}

typeof_decl (ExprDecl expr) = do
  (expr', ty) <- typeof_expr expr
  return (ExprDecl expr', ty)

typeof_expr :: Expr String -> TcRes Expr
typeof_expr (LiteralExpr lit) = do
  (lit', ty) <- typeof_literal lit
  return (LiteralExpr lit', ty)

typeof_expr (FunctionExpr fn) = do
  (fn', ty) <- typeof_function fn
  return (FunctionExpr fn', ty)

typeof_expr (Arg (Loc pos name) index) = do
  ty <- typeof_ident pos name
  return (Arg (Loc pos (TcId name ty)) index, ty)

typeof_expr (Var (Loc pos name)) = do
  ty <- typeof_ident pos name
  return (Var (Loc pos (TcId name ty)), ty)

{-typeof_expr BinaryOp { lhs=lhs, rhs=rhs } = do-}
  {-lhs' <- typeof_expr lhs-}
  {-rhs' <- typeof_expr rhs-}
  {-case (lhs', rhs') of-}
    {-(TyInt, TyInt) -> return TyInt-}
    {-(TyFloat, TyFloat) -> return TyFloat-}
    {-(_, _) -> throwError (pos, "Binary operations can only happen on two integers or two floats")-}


typeof_expr (Call callee (Loc pos args)) = do
  ctx <- get
  (callee', callee_type) <- typeof_expr callee
  let (params, ret_type) = case callee_type of
                             (TyFunction params ret_type) -> (params, ret_type)
                             (TyAbstractFunction (TyFunction params ret_type) _) -> (params, ret_type)
  when (length params /= length args) (throwError (pos, "Wrong number of arguments for function call"))
  (args', ty_args) <- liftM unzip $ mapM typeof_expr args
  mapM_ (uncurry (tyeqv pos)) (zip params ty_args)
  case callee_type of
    (TyAbstractFunction _ interface_name) -> do
      (Just (TyInterface {ty_name=name, ty_variable=var, ty_implementations=impls})) <- gets $ Map.lookup interface_name
      t <- gets $ Map.lookup var
      when (case fromJust t of (TyEmptyGeneric _) -> True; _ -> False) (throwError (pos, "Undecidable abstract function call"))
      when (not $ (fromJust t) `elem` impls) (throwError (pos, printf "Implementation not found: interface `%s`, impl_type `%s`, impls: `%s`" name (show $ fromJust t) (show impls)))
      return ()
    _ -> return ()
  put ctx
  return (Call callee' (Loc pos args'), ret_type)

typeof_expr expr = throwError (SourcePos 0 0 "/tmp/foo.vrv", "Unsupported expr: " ++ (show expr))

typeof_ident :: SourcePos -> String -> TcState TyType
typeof_ident pos name = do
  value <- gets (Map.lookup name)
  maybe throw return value
    where throw = throwError (pos, printf "Unknown identifier: `%s`" name)

typeof_literal :: Literal -> TcState (Literal, TyType)
typeof_literal lit@(Number (Left  _))  = return (lit, TyInt)
typeof_literal lit@(Number (Right  _)) = return (lit, TyFloat)
typeof_literal lit@(String _)          = return (lit, TyString)

typeof_fn_type :: FnType String -> TcRes FnType
typeof_fn_type (FnType vars params ret_type) = do
  (params', ty_params) <- liftM unzip $ mapM typeof_type params
  (ret_type', ty_ret_type) <- typeof_type ret_type
  {-TODO: fix function variables-}
  return (FnType (Just []) params' ret_type', TyFunction ty_params ty_ret_type)

typeof_type :: Type String -> TcRes Type
typeof_type (BasicType (Loc pos t)) = do
  value <- gets (Map.lookup t)
  t' <- case t of
         "char" -> return TyChar
         "int" -> return TyInt
         "float" -> return TyFloat
         "void" -> return TyVoid
         {- TODO: Declare types on prelude -}
         "bool" -> return TyBool
         "string" -> return TyString
         _ -> maybe throw return value
           where throw = throwError (pos, printf "Unknown type: `%s`" t)
  return (BasicType (Loc pos (TcId t t')), t')

typeof_function :: Function String -> TcRes Function
typeof_function Function { fn_name=(Loc pos name), params=params, variables=variables, ret_type=(Just ret_type), body=body } = do
  ctx <- get
  load_type_variables variables
  (params', ty_params) <- liftM unzip $ mapM typeof_fn_param params
  (body', ty_body) <- typeof_block body
  (ret_type', ty_ret) <- typeof_type ret_type
  tyeqv pos ty_body ty_ret
  let t = TyFunction ty_params ty_ret
  put (Map.insert name t ctx)
  return (Function (Loc pos (TcId name t)) (Nothing) params' (Just ret_type') body', t)

{-typeless function-}
typeof_function fn@Function { fn_name=(Loc pos name), params=params, body=body } = do
  t <- gets $ Map.lookup name
  when (isNothing t) (throwError (pos, "Typeless function without prior declaration"))
  let (TyAbstractFunction (TyFunction ty_params ty_ret) _) = fromJust t
  let params' = map (\((FunctionParameter (Loc pos name) index Nothing), ty_param) ->
                      FunctionParameter (Loc pos (TcId name ty_param)) index Nothing) (zip params ty_params)

  (body', ty_body) <- typeof_block body
  tyeqv pos ty_ret ty_body
  return (Function (Loc pos (TcId name $ fromJust t)) Nothing params' Nothing body', fromJust t)

typeof_block :: Block String -> TcRes Block
typeof_block (Block exprs) = do
  (exprs', ty_exprs) <- liftM unzip $ mapM typeof_expr exprs
  let t = case ty_exprs of
            [] -> TyVoid
            _ -> last ty_exprs
  return (Block exprs', t)

typeof_fn_param :: FunctionParameter String -> TcRes FunctionParameter
typeof_fn_param (FunctionParameter (Loc pos name) index (Just param_type)) = do
  (param_type', ty_param) <- typeof_type param_type
  modify $ Map.insert name ty_param
  return (FunctionParameter (Loc pos (TcId name ty_param)) index (Just param_type'), ty_param)

typeof_interface :: Interface String -> TcRes Interface
typeof_interface Interface { interface_name=name, interface_var=var, interface_functions=fns } = do
  var' <- uniquify var
  (fns', ty_fns) <- liftM unzip $ mapM typeof_interface_fn fns
  let interface = (TyInterface name var' ty_fns [])
  modify $ Map.insert name interface
  modify $ Map.insert var' (TyEmptyGeneric var')
  ctx <- get
  mapM (\(fn, t)->
    let fn_name = case fn of (AbstractFunction (Prototype name _)) -> name
     in modify $ Map.insert fn_name (TyAbstractFunction t name)
     ) (zip fns ty_fns)
  return (Interface (TcId name interface) (TcId var' TyVoid) fns', TyVoid)

typeof_interface_fn :: InterfaceFunction String -> TcRes InterfaceFunction
typeof_interface_fn  (AbstractFunction proto) = do
  (proto', ty_proto) <- typeof_prototype proto
  return (AbstractFunction proto', ty_proto)

typeof_interface_fn (ConcreteFunction fn) = do
  (fn', ty_fn) <- typeof_function fn
  return (ConcreteFunction fn', ty_fn)

typeof_prototype :: Prototype String -> TcRes Prototype
typeof_prototype (Prototype name signature) = do
  (proto, t) <- typeof_fn_type signature
  modify $ Map.insert name t
  return (Prototype (TcId name t) proto, t)

typeof_implementation :: Implementation String -> TcRes Implementation
typeof_implementation Implementation { target_interface=(Loc pos name), implementation_type=t, implementation_functions=fns } = do
  ctx <- get
  interface <- gets $ Map.lookup name
  when (isNothing interface) (throwError (pos, "Interface for implementation not found"))
  let ty_interface = fromJust interface

  (t', ty_t) <- typeof_type t
  modify $ Map.insert (ty_variable ty_interface) ty_t
  (fns', ty_fns) <- liftM unzip $ mapM typeof_impl_fn fns

  put $ Map.adjust (\i -> i { ty_implementations=ty_t:(ty_implementations i) }) name ctx
  return (Implementation (Loc pos (TcId name ty_t)) t' fns', TyVoid)

typeof_impl_fn :: ImplementationFunction String -> TcRes ImplementationFunction
typeof_impl_fn (ExternImplementation (Loc pos name)) = do
  t <- gets $ Map.lookup name
  when (isNothing t) (throwError (pos, "Externing unknown function"))
  let t' = fromJust t
  return (ExternImplementation (Loc pos (TcId name t')), t')

typeof_impl_fn (LocalImplementation fn) = do
  (fn', ty_fn) <- typeof_function fn
  return (LocalImplementation fn', ty_fn)

tyeqv :: SourcePos -> TyType -> TyType -> TcState ()
tyeqv pos t1 t2 = do
  t1' <- simplify t1
  t2' <- simplify t2
  ctx <- get
  case (t1', t2') of
    (t, TyEmptyGeneric a) -> do
      put $ Map.insert a t ctx
      return ()
    (TyEmptyGeneric a, t) -> do
      put $ Map.insert a t ctx
      return ()
    (t, TyGeneric a) | fromJust (Map.lookup a ctx) /= t2 -> do
      let a' = fromJust (Map.lookup a ctx)
       in tyeqv pos t a'
    (TyGeneric a, t) | fromJust (Map.lookup a ctx) /= t1 -> do
      let a' = fromJust (Map.lookup a ctx)
       in tyeqv pos a' t
    (_, _) ->
      if t1' == t2'
      then return ()
      else throwError (pos, printf "Invalid type: expected `%s` but received `%s`" (show t1') (show t2'))

simplify :: TyType -> TcState TyType
{-simplify generic@(TyGeneric name) = do-}
  {-t <- gets $ Map.lookup name-}
  {-case t of-}
    {-Nothing -> return generic-}
    {-Just t -> simplify t-}
simplify t = return t

load_type_variables :: Maybe [String] -> TcState ()
load_type_variables Nothing = return ()
load_type_variables (Just vars) = do
  ctx <- get
  ctx' <- foldlM add_var ctx vars
  put ctx'
  return ()
  -- add_var :: Context -> String -> (MonadState ())
    where add_var = \ctx str -> do {
          str' <- uniquify str;
          return $ Map.insert str' (TyEmptyGeneric str') ctx
                                   }

uniquify :: String -> (TcState String)
uniquify var = uniquify' var var

uniquify' :: String -> String -> (TcState String)
uniquify' orig var = do
  t <- gets $ Map.lookup var
  case t of
    Nothing -> do
      modify $ Map.insert orig (TyGeneric var)
      return var

    Just t -> uniquify' orig (var ++ "'")
