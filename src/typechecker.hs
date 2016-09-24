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

type Context = (Map.Map String Type)
type Error = (SourcePos, String)
type TypeCheckerMonad = ExceptT Error (State Context)
type TypeCheckerState = TypeCheckerMonad Type

type_check :: AST -> Either (SourcePos, String) Context
type_check node = evalState (runExceptT $ typeof node >> get) Map.empty

typeof :: AST -> TypeCheckerState
typeof Program { expressions=nodes } =
  foldlM (flip $ const . typeof) TyVoid nodes

typeof Block { nodes=nodes } =
  foldlM (flip $ const . typeof) TyVoid nodes

typeof Number { num_value=(Left  _) } = return TyInt
typeof Number { num_value=(Right _) } = return TyFloat
typeof String {} = return TyString

typeof Identifier { pos=pos, name=name } = do
  value <- gets (Map.lookup name)
  maybe throw return value
    where throw = throwError (pos, printf "Unknown identifier: `%s`" name)

typeof BasicType { pos=pos, type_name=t } = do
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

typeof Function { pos=pos, name=name, params=params, variables=variables, ret_type=(Just ret_type), body=body } = do
  ctx <- get
  load_type_variables variables
  ty_body <- typeof body
  ty_ret <- typeof ret_type
  tyeqv pos ty_body ty_ret
  t <- function_type params ret_type
  put (Map.insert name t ctx)
  return t

{-typeless function-}
typeof Function { pos=pos, name=name } = do
  t <- gets $ Map.lookup name
  when (isNothing t) (throwError (pos, "Typeless function without prior declaration"))
  return (fromJust t)

typeof FunctionType { parameters=params, return_type=ret_type } =
  function_type params ret_type

typeof Extern  { prototype=prototype } = do
  t <- typeof prototype
  modify $ Map.insert (name prototype) t
  return t

typeof Virtual { prototype=prototype } = typeof prototype

typeof Prototype { prototype=fn_type } = typeof fn_type

typeof Call { pos=pos, callee=callee, arguments=args } = do
  ctx <- get
  callee_type <- typeof callee
  let (params, ret_type) = case callee_type of
                             (TyFunction params ret_type) -> (params, ret_type)
                             (TyAbstractFunction (TyFunction params ret_type) _) -> (params, ret_type)
  when (length params /= length args) (throwError (pos, "Wrong number of arguments for function call"))
  args' <- mapM typeof args
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

typeof FunctionParameter { type'=(Just t) } = typeof t

typeof BinaryOp { pos=pos, lhs=lhs, rhs=rhs } = do
  lhs' <- typeof lhs
  rhs' <- typeof rhs
  case (lhs', rhs') of
    (TyInt, TyInt) -> return TyInt
    (TyFloat, TyFloat) -> return TyFloat
    (_, _) -> throwError (pos, "Binary operations can only happen on two integers or two floats")

typeof Interface { name=name, variable=var, functions=fns } = do
  var' <- uniquify var
  fns' <- mapM typeof fns
  let interface = (TyInterface name var' fns' [])
  modify $ Map.insert name interface
  modify $ Map.insert var' (TyEmptyGeneric var')
  ctx <- get
  mapM (\(fn, t)->
    let fn_name = case fn of Virtual { prototype=Prototype {name=name} } -> name
     in modify $ Map.insert fn_name (TyAbstractFunction t name)
     ) (zip fns fns')
  return TyVoid

typeof Implementation { pos=pos, name=name, impl_type=t', functions=fns } = do
  ctx <- get
  interface <- gets $ Map.lookup name
  when (isNothing interface) (throwError (pos, "Interface for implementation not found"))

  t <- typeof t'
  modify $ Map.insert (ty_variable $ fromJust interface) t
  mapM_ typeof fns

  let i = fromJust interface
  put $ Map.adjust (\i -> i { ty_implementations=t:(ty_implementations i) }) name ctx
  ctx' <- get
  return TyVoid

typeof t = throwError (SourcePos { file="", line=0, column=0 }, "Unhandled node: " ++ (show t))

tyeqv :: SourcePos -> Type -> Type -> (TypeCheckerMonad ())
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

simplify :: Type -> TypeCheckerState
simplify generic@(TyGeneric name) = do
  t <- gets $ Map.lookup name
  case t of
    Nothing -> return generic
    Just t -> simplify t
simplify t = return t

function_type :: [AST] -> AST -> TypeCheckerState
function_type params ret_type =
  TyFunction <$> (mapM typeof params) <*> (typeof ret_type)

load_type_variables :: Maybe [String] -> (TypeCheckerMonad ())
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

uniquify :: String -> (TypeCheckerMonad String)
uniquify var = uniquify' var var

uniquify' :: String -> String -> (TypeCheckerMonad String)
uniquify' orig var = do
  t <- gets $ Map.lookup var
  case t of
    Nothing -> do
      modify $ Map.insert orig (TyGeneric var)
      return var
    Just t -> uniquify' orig (var ++ "'")
