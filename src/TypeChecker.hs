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
  return TyVoid

typeof_decl (ImplementationDecl implementation) =
  return TyVoid

typeof_decl (ExternDecl (Prototype name signature)) = do
  fn_type <- typeof_fn_type signature
  modify $ Map.insert name fn_type
  return fn_type

typeof_decl (TypeDecl enum_type) =
  return TyVoid

typeof_decl (ExprDecl expr) = typeof_expr expr

typeof_expr :: Expr String -> TCState TyType
typeof_expr (LiteralExpr lit) = typeof_literal lit

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

typeof_literal :: Literal String -> TCState TyType
typeof_literal (Number (Left  _))  = return TyInt
typeof_literal (Number (Right  _)) = return TyFloat
typeof_literal (String _)          = return TyString
typeof_literal (Identifier (Loc pos name))   = do
  value <- gets (Map.lookup name)
  maybe throw return value
    where throw = throwError (pos, printf "Unknown identifier: `%s`" name)

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

{-typeof Function { fn_name=name, params=params, variables=variables, ret_type=(Just ret_type), body=body } = do-}
  {-ctx <- get-}
  {-load_type_variables variables-}
  {-ty_body <- typeof body-}
  {-ty_ret <- typeof ret_type-}
  {-tyeqv pos ty_body ty_ret-}
  {-t <- function_type params ret_type-}
  {-put (Map.insert name t ctx)-}
  {-return t-}

{-typeless function-}
{-typeof Function { fn_name=name } = do-}
  {-t <- gets $ Map.lookup name-}
  {-when (isNothing t) (throwError ("Typeless function without prior declaration"))-}
  {-return (fromJust t)-}

{-typeof FunctionType { parameters=params, return_type=ret_type } =-}
  {-function_type params ret_type-}

{-typeof BinaryOp { lhs=lhs, rhs=rhs } = do-}
  {-lhs' <- typeof lhs-}
  {-rhs' <- typeof rhs-}
  {-case (lhs', rhs') of-}
    {-(TyInt, TyInt) -> return TyInt-}
    {-(TyFloat, TyFloat) -> return TyFloat-}
    {-(_, _) -> throwError ("Binary operations can only happen on two integers or two floats")-}

{-typeof Interface { name=name, variable=var, functions=fns } = do-}
  {-var' <- uniquify var-}
  {-fns' <- mapM typeof fns-}
  {-let interface = (TyInterface name var' fns' [])-}
  {-modify $ Map.insert name interface-}
  {-modify $ Map.insert var' (TyEmptyGeneric var')-}
  {-ctx <- get-}
  {-mapM (\(fn, t)->-}
    {-let fn_name = case fn of Virtual { prototype=Prototype {name=name} } -> name-}
     {-in modify $ Map.insert fn_name (TyAbstractFunction t name)-}
     {-) (zip fns fns')-}
  {-return TyVoid-}

{-typeof Implementation { name=name, impl_type=t', functions=fns } = do-}
  {-ctx <- get-}
  {-interface <- gets $ Map.lookup name-}
  {-when (isNothing interface) (throwError ("Interface for implementation not found"))-}

  {-t <- typeof t'-}
  {-modify $ Map.insert (ty_variable $ fromJust interface) t-}
  {-mapM_ typeof fns-}

  {-let i = fromJust interface-}
  {-put $ Map.adjust (\i -> i { ty_implementations=t:(ty_implementations i) }) name ctx-}
  {-ctx' <- get-}
  {-return TyVoid-}

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
