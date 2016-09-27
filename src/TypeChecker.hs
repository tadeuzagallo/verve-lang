module TypeChecker (type_check, Context) where

import AST
import Type

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (State, evalState, get, gets, put, modify)
import qualified Data.Map as Map
import Data.Foldable (foldlM)
import Data.Maybe (isNothing, fromJust)
import Text.Printf (printf)

type Context = (Map.Map String TyType)
type Error = (SourcePos, String)
type TCState = ExceptT Error (State Context)

type_check :: Program String -> Either Error Context
type_check program =
  evalState (runExceptT $ typeof_program program >> get) Map.empty

typeof_program :: Program String -> TCState TyType
typeof_program (Program imports decls) = do
  mapM_ typeof_decl decls
  return TyVoid

typeof_decl (InterfaceDecl interface) =
   typeof_interface interface

typeof_decl (ImplementationDecl implementation) =
  typeof_implementation implementation

typeof_decl (ExternDecl (Prototype name signature)) = do
  fn_type <- typeof_fn_type signature
  modify $ Map.insert name fn_type
  return fn_type

typeof_decl (TypeDecl enum_type) =
  return TyVoid

typeof_decl (ExprDecl expr) = typeof_expr expr

typeof_expr :: Expr String -> TCState TyType
typeof_expr (LiteralExpr lit) = typeof_literal lit

typeof_expr (FunctionExpr fn) = typeof_function fn

typeof_expr (Arg (Loc pos name) _) =
  typeof_ident pos name

typeof_expr (Var (Loc pos name)) =
  typeof_ident pos name

{-typeof_expr BinaryOp { lhs=lhs, rhs=rhs } = do-}
  {-lhs' <- typeof_expr lhs-}
  {-rhs' <- typeof_expr rhs-}
  {-case (lhs', rhs') of-}
    {-(TyInt, TyInt) -> return TyInt-}
    {-(TyFloat, TyFloat) -> return TyFloat-}
    {-(_, _) -> throwError (pos, "Binary operations can only happen on two integers or two floats")-}


typeof_expr (Call callee (Loc pos args)) = do
  ctx <- get
  callee_type <- typeof_expr callee
  let (params, ret_type) = case callee_type of
                             (TyFunction params ret_type) -> (params, ret_type)
                             (TyAbstractFunction (TyFunction params ret_type) _) -> (params, ret_type)
  when (length params /= length args) (throwError (pos, "Wrong number of arguments for function call"))
  args' <- mapM typeof_expr args
  mapM_ (uncurry (tyeqv pos)) (zip params args')
  case callee_type of
    (TyAbstractFunction _ interface_name) -> do
      (Just (TyInterface {ty_name=name, ty_variable=var, ty_implementations=impls})) <- gets $ Map.lookup interface_name
      t <- gets $ Map.lookup var
      when (case fromJust t of (TyEmptyGeneric _) -> True; _ -> False) (throwError (pos, "Undecidable abstract function call"))
      when (not $ (fromJust t) `elem` impls) (throwError (pos, printf "Implementation not found: interface `%s`, impl_type `%s`, impls: `%s`" name (show $ fromJust t) (show impls)))
      return ()
    _ -> return ()
  put ctx
  return ret_type

typeof_expr expr = throwError (SourcePos 0 0 "/tmp/foo.vrv", "Unsupported expr: " ++ (show expr))

typeof_ident :: SourcePos -> String -> TCState TyType
typeof_ident pos name = do
  value <- gets (Map.lookup name)
  maybe throw return value
    where throw = throwError (pos, printf "Unknown identifier: `%s`" name)

typeof_literal :: Literal String -> TCState TyType
typeof_literal (Number (Left  _))  = return TyInt
typeof_literal (Number (Right  _)) = return TyFloat
typeof_literal (String _)          = return TyString

typeof_fn_type :: FnType String -> TCState TyType
typeof_fn_type (FnType _  params ret_type) = do
  TyFunction <$> mapM typeof_type params <*> typeof_type ret_type

typeof_type :: Type String -> TCState TyType
typeof_type (BasicType (Loc pos t)) = do
  value <- gets (Map.lookup t)
  case t of
    "char" -> return TyChar
    "int" -> return TyInt
    "float" -> return TyFloat
    "void" -> return TyVoid
    {- TODO: Declare types on prelude -}
    "bool" -> return TyBool
    "string" -> return TyString
    _ -> maybe throw return value
      where throw = throwError (pos, printf "Unknown type: `%s`" t)

typeof_function :: Function String -> TCState TyType
typeof_function Function { fn_name=(Loc pos name), params=params, variables=variables, ret_type=(Just ret_type), body=body } = do
  ctx <- get
  load_type_variables variables
  ty_params <- mapM typeof_fn_param params
  ty_body <- typeof_block body
  ty_ret <- typeof_type ret_type
  tyeqv pos ty_body ty_ret
  let t = TyFunction ty_params ty_ret
  put (Map.insert name t ctx)
  return t

{-typeless function-}
typeof_function Function { fn_name=(Loc pos name) } = do
  t <- gets $ Map.lookup name
  when (isNothing t) (throwError (pos, "Typeless function without prior declaration"))
  return (fromJust t)

typeof_block :: Block String -> TCState TyType
typeof_block (Block exprs) =
  foldlM (flip $ const . typeof_expr) TyVoid exprs

typeof_fn_param :: FunctionParameter String -> TCState TyType
typeof_fn_param (FunctionParameter (Loc _ name) _ (Just param_type)) = do
  ty_param <- typeof_type param_type
  modify $ Map.insert name ty_param
  return ty_param

typeof_interface :: Interface String -> TCState TyType
typeof_interface Interface { interface_name=name, interface_var=var, interface_functions=fns } = do
  var' <- uniquify var
  fns' <- mapM typeof_interface_fn fns
  let interface = (TyInterface name var' fns' [])
  modify $ Map.insert name interface
  modify $ Map.insert var' (TyEmptyGeneric var')
  ctx <- get
  mapM (\(fn, t)->
    let fn_name = case fn of (AbstractFunction (Prototype name _)) -> name
     in modify $ Map.insert fn_name (TyAbstractFunction t name)
     ) (zip fns fns')
  return TyVoid

typeof_interface_fn :: InterfaceFunction String -> TCState TyType
typeof_interface_fn  (AbstractFunction proto) =
  typeof_prototype proto

typeof_interface_fn (ConcreteFunction fn) =
  typeof_function fn

typeof_prototype :: Prototype String -> TCState TyType
typeof_prototype (Prototype name signature) = do
  t <- typeof_fn_type signature
  modify $ Map.insert name t
  return t

typeof_implementation :: Implementation String -> TCState TyType
typeof_implementation Implementation { target_interface=(Loc pos name), implementation_type=t', implementation_functions=fns } = do
  ctx <- get
  interface <- gets $ Map.lookup name
  when (isNothing interface) (throwError (pos, "Interface for implementation not found"))

  t <- typeof_type t'
  modify $ Map.insert (ty_variable $ fromJust interface) t
  mapM_ typeof_impl_fn fns

  let i = fromJust interface
  put $ Map.adjust (\i -> i { ty_implementations=t:(ty_implementations i) }) name ctx
  ctx' <- get
  return TyVoid

typeof_impl_fn :: ImplementationFunction String -> TCState TyType
typeof_impl_fn (ExternImplementation (Loc pos name)) = do
  t <- gets $ Map.lookup name
  maybe throw return t
    where throw = (throwError (pos, "Externing unknown function"))

typeof_impl_fn (LocalImplementation fn) =
  typeof_function fn

tyeqv :: SourcePos -> TyType -> TyType -> TCState ()
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

simplify :: TyType -> TCState TyType
{-simplify generic@(TyGeneric name) = do-}
  {-t <- gets $ Map.lookup name-}
  {-case t of-}
    {-Nothing -> return generic-}
    {-Just t -> simplify t-}
simplify t = return t

load_type_variables :: Maybe [String] -> TCState ()
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

uniquify :: String -> (TCState String)
uniquify var = uniquify' var var

uniquify' :: String -> String -> (TCState String)
uniquify' orig var = do
  t <- gets $ Map.lookup var
  case t of
    Nothing -> do
      modify $ Map.insert orig (TyGeneric var)
      return var

    Just t -> uniquify' orig (var ++ "'")
