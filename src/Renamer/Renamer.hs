module Renamer.Renamer
  ( renameImport
  , renameStmts
  ) where

import Absyn.Untyped
import Renamer.Env
import Typing.TypeError
import Util.Error
import Util.Scope

import Control.Monad (zipWithM)
import Control.Monad.Except (withExcept)
import Control.Monad.State (mapStateT)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)


-- ENTRY POINT

renameStmts :: String -> [Stmt] -> RnEnv -> Result (RnEnv, [Stmt])
renameStmts modName stmts state = do
  runRn modName (r_stmts stmts) state

renameImport :: RnEnv -> RnEnv -> Import -> (RnEnv, [String])
renameImport targetEnv importEnv (Import isGlobal mod alias items) =
  let env' = foldl (importValue importEnv) targetEnv valuesToImport
      env'' = foldl (importType importEnv) env' typesToImport
   in (env'', map fullName (valuesToImport ++ typesToImport))
  where
    (valuesToImport, typesToImport) =
      case items of
        Just items -> foldl processItem ([], []) items
        Nothing -> findValuesAndTypes modName modAlias importEnv

    processItem (values, types) (ImportValue v) = (rdrName v : values, types)
    processItem (values, types) (ImportType t vs) = (map rdrName vs ++ values, rdrName t : types)

    rdrName name = (modAlias, name)

    modName = intercalate "." mod

    -- returns the `name` with the full module name, not the local alias
    fullName (_moduleAlias, name) = joinName (modName, name)

    -- the alias under which the imports should be bound to in the new env
    modAlias | isGlobal = Nothing
             | otherwise = Just $ fromMaybe modName alias


-- ABSYN RENAMERS

r_stmts :: [Stmt] -> Rn [Stmt]
r_stmts stmts = mapM r_stmt stmts

r_body :: [Stmt] -> Rn [Stmt]
r_body stmts = do
  m <- startMarker
  stmts' <- r_stmts stmts
  endMarker m
  clearMarker m
  return stmts'

r_stmt :: Stmt -> Rn Stmt
r_stmt (Expr expr) =
  Expr <$> r_expr expr

r_stmt (Decl decl) =
  Decl <$> r_decl decl

r_decl :: Decl -> Rn Decl
r_decl (Enum name gen ctors) = do
  insertLocalType name

  name' <- local name

  m <- startMarker
  mapM_ insertInternalType gen
  endMarker m

  ctors' <- r_ctors ctors

  clearMarker m

  return $ Enum name' gen ctors'

r_decl (FnStmt fn) = do
  insertLocalValue (name fn)

  FnStmt <$> r_fn fn

r_decl (Let (name, ty) expr) = do
  insertLocalValue name

  name' <- local name
  ty' <- r_type ty
  expr' <- r_expr expr
  return $ Let (name', ty') expr'

r_decl (Class name vars methods) = do
  insertLocalType name
  insertLocalValue name

  name' <- local name

  m <- startMarker
  vars' <- r_fnParams vars
  endMarker m

  names' <- mapM r_methodSig methods
  methods' <- zipWithM r_methodImp names' methods

  clearMarker m

  return $ Class name' vars' methods'

r_decl (Operator assoc prec gen lhs op rhs retType body) = do
  insertLocalValue op

  prec' <- r_prec prec
  op' <- local op

  m <- startMarker
  gen' <- r_generics gen
  [lhs', rhs'] <- r_fnParams [lhs, rhs]
  endMarker m

  retType' <- r_type retType

  body' <- r_body body

  clearMarker m

  return $ Operator assoc prec' gen' lhs' op' rhs' retType' body'

r_decl (Interface name param methods) = do
  insertLocalType name

  name' <- local name

  m <- startMarker
  insertInternalType param
  endMarker m

  methods' <- mapM r_intfField methods

  clearMarker m

  return $ Interface name' param methods'

r_decl (Implementation name gen ty implMethods) = do
  name' <- renameType name

  m <- startMarker
  gen' <- r_generics gen
  endMarker m

  ty' <- r_type ty
  implMethods' <- mapM r_implItem implMethods

  clearMarker m

  return $ Implementation name' gen' ty' implMethods'

r_decl (TypeAlias aliasName aliasVars aliasType) = do
  insertLocalType aliasName

  aliasName' <- local aliasName

  m <- startMarker
  mapM_ insertInternalType aliasVars
  endMarker m

  aliasType' <- r_type aliasType

  clearMarker m

  return $ TypeAlias aliasName' aliasVars aliasType'


r_implItem :: ImplementationItem -> Rn ImplementationItem
r_implItem (ImplVar (name, expr)) = do
  name' <- lookupIntfMethod name
  expr' <- r_expr expr
  return $ ImplVar (name', expr')

r_implItem (ImplFunction implName implParams implBody) = do
  implName' <- lookupIntfMethod implName

  m <- startMarker
  mapM_ insertInternalValue implParams
  endMarker m

  implBody' <- r_body implBody

  clearMarker m

  return $ ImplFunction implName' implParams implBody'

r_implItem (ImplOperator lhs op rhs body) = do
  op' <- lookupIntfMethod op

  m <- startMarker
  insertInternalValue lhs
  insertInternalValue rhs
  endMarker m

  body' <- r_body body

  clearMarker m

  return $ ImplOperator lhs op' rhs body'

lookupIntfMethod :: String -> Rn String
lookupIntfMethod name = do
  mapStateT (withExcept $ \_ -> Error (ExtraneousImplementation name)) $ renameValue name

r_expr :: Expr -> Rn Expr
r_expr (Literal l) =
  return $ Literal l

r_expr (ParenthesizedExpr expr) =
  ParenthesizedExpr <$> r_expr expr

r_expr (FnExpr fn) = do
  FnExpr <$> r_fn fn

r_expr (Ident name ty) = do
  name' <- renameIdentValue name
  ty' <- r_type ty
  return $ Ident [name'] ty'

r_expr (Match expr cases) = do
  expr' <- r_expr expr
  cases' <- mapM r_case cases
  return $ Match expr' cases'

r_expr (If cond conseq alt) = do
  cond' <- r_expr cond
  conseq' <- r_body conseq
  alt' <- r_body alt
  return $ If cond' conseq' alt'

r_expr (Call callee _ tyArgs vArgs) = do
  callee' <- r_expr callee
  tyArgs' <- mapM r_type tyArgs
  vArgs' <- mapM r_expr vArgs
  return $ Call callee' [] tyArgs' vArgs'

r_expr (BinOp _ _ lhs op rhs) = do
  lhs' <- r_expr lhs
  op' <- renameValue op
  rhs' <- r_expr rhs
  return $ BinOp [] [] lhs' op' rhs'

r_expr (Record fields) = do
  fields' <- mapM (sequence . fmap r_expr) fields
  return $ Record fields'

r_expr (List ty items) = do
  items' <- mapM r_expr items
  return $ List ty items'

r_expr (FieldAccess expr ty field) = do
  expr' <- r_expr expr
  ty' <- r_type ty
  return $ FieldAccess expr' ty' field

r_expr (Negate constrArgs expr) =
  Negate constrArgs <$> r_expr expr

-- Expressions that can only be generated by the compiler after Type Checking
r_expr VoidExpr = undefined
r_expr (TypeCall {}) = undefined


r_case :: Case -> Rn Case
r_case (Case pat body) = do
  m <- startMarker
  pat' <- r_pat pat
  endMarker m

  body' <- r_body body

  clearMarker m

  return $ Case pat' body'


r_pat :: Pattern -> Rn Pattern
r_pat PatDefault =
  return PatDefault

r_pat (PatLiteral l) =
  return $ PatLiteral l

r_pat (PatVar v) = do
  insertInternalValue v
  return $ PatVar v

r_pat (PatRecord fields) = do
  fields' <- mapM (sequence . fmap r_pat) fields
  return $ PatRecord fields'

r_pat (PatList pats rest) = do
  pats' <- mapM r_pat pats
  case rest of
    NamedRest n -> insertInternalValue n
    _ -> return ()
  -- TODO: This is a bug, it had reverse order at some point as is being re-reversed somewhere else
  return $ PatList (reverse pats') rest

r_pat (PatCtor name args) = do
  name' <- renameValue name
  args' <- mapM r_pat args
  return $ PatCtor name' args'

r_type :: Type -> Rn Type
r_type TVoid =
  return TVoid

r_type TPlaceholder =
  return TPlaceholder

r_type (TName name) =
  TName <$> renameType name

r_type (TApp ty args) = do
  ty' <- r_type ty
  args' <- mapM r_type args
  return $ TApp ty' args'

r_type (TArrow params ret) = do
  params' <- mapM r_type params
  ret' <- r_type ret
  return $ TArrow params' ret'

r_type (TRecord fields) = do
  fields' <- mapM (sequence . fmap r_type) fields
  return $ TRecord fields'

r_fn :: Function -> Rn Function
r_fn fn = do
  m <- startMarker
  insertLocalValue (name fn)
  endMarker m

  name' <- local (name fn)
  fn' <- r_fnBase name' fn

  clearMarker m

  return fn'

r_fnBase :: String -> Function -> Rn Function
r_fnBase name' (Function _ gen params retType body) = do
  m <- startMarker
  gen' <- r_generics gen
  params' <- r_fnParams params
  endMarker m

  retType' <- r_type retType
  body' <- r_body body

  clearMarker m

  return $ Function name' gen' params' retType' body'


r_fnParams :: [Param] -> Rn [Param]
r_fnParams = mapM r_fnParam


r_fnParam :: Param -> Rn Param
r_fnParam (name, ty) = do
  insertInternalValue name
  ty' <- r_type ty
  return (name, ty')


r_generics :: Generics -> Rn Generics
r_generics = mapM r_generic


r_generic :: (Name, [Name]) -> Rn (Name, [Name])
r_generic (name, bounds) = do
  insertInternalType name
  bounds' <- mapM renameType bounds
  return (name, bounds')


r_ctors :: [DataCtor] -> Rn [DataCtor]
r_ctors = mapM r_ctor


r_ctor :: DataCtor -> Rn DataCtor
r_ctor (name, args) = do
  insertLocalValue name
  name' <- local name
  args' <- mapM (mapM r_type) args
  return (name', args')


r_prec :: Precedence -> Rn Precedence
r_prec (PrecValue v) =
  return $ PrecValue v

r_prec (PrecHigher name) = do
  PrecHigher <$> renameValue name

r_prec (PrecLower name) = do
  PrecLower <$> renameValue name

r_prec (PrecEqual name) = do
  PrecEqual <$> renameValue name


r_methodSig :: Function -> Rn String
r_methodSig fn = do
  insertLocalValue (name fn)
  local (name fn)

r_methodImp :: String -> Function -> Rn Function
r_methodImp name' fn = do
  insertInternalValue "self"
  r_fnBase name' fn

r_intfField :: InterfaceItem -> Rn InterfaceItem
r_intfField (IntfVar (name, ty)) = do
  insertLocalValue name

  name' <- local name
  ty' <- r_type ty
  return $ IntfVar (name', ty')

r_intfField (IntfOperator assoc prec lhs op rhs ret) = do
  insertLocalValue op

  prec' <- r_prec prec
  lhs' <- r_type lhs
  op' <- local op
  rhs' <- r_type rhs
  ret' <- r_type ret

  return $ IntfOperator assoc prec' lhs' op' rhs' ret'
