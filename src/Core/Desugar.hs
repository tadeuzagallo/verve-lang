{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Core.Desugar
  ( DsState
  , importDsState
  , initialState
  , desugarStmts
  , d_expr
  ) where

import Core.State
import {-# SOURCE #-} Core.Match

import Absyn.Typed
import qualified Absyn.Untyped as U
import Typing.Types
import qualified Core.Absyn as CA

import Control.Monad (foldM)
import Data.List (groupBy)

desugarStmts :: DsState -> [Stmt] -> (DsState, CA.Term)
desugarStmts state stmts =
  runDesugar (d_stmts stmts) state

dummySpan :: SourceSpan
dummySpan = SourceSpan { spanStart = error "unknown span start"
                       , spanEnd = error "unknown span end"
                       }

d_stmts :: [Stmt] -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_stmts stmts k =
  let stmts' = groupBy f stmts
   in g stmts' k
    where
      f (_ :< Decl _) (_ :< Decl _) = True
      f (_ :< Expr _) (_ :< Expr _) = True
      f _ _ = False

      g (ds@(_ :< Decl _: _) : rest) k = do
        let r =
              case rest of
                [] -> k
                r -> \_ -> g r k
        d_decls (map (\(_ :< Decl d) -> d) ds) r

      g (es@(_ :< Expr _ : _) : rest) k = do
        let init = case rest of
                     [] -> \x -> k x
                     r -> \_ -> g r k
        foldr (\e k _ -> d_expr e k) init [e | _ :< Expr e <- es] []

      g [] k =
        d_expr (dummySpan :< VoidExpr) k

d_codeBlock :: CodeBlock -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_codeBlock (_ :< CodeBlock stmts) k =
  d_stmts stmts k

d_decls :: [Decl] -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_decls [d] k =
  d_decl d k

d_decls (d:ds) k = do
  d_decl d (\_ -> d_decls ds k)

d_decl :: Decl -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_decl (_ :< Let (x, _) expr) k = do
  j <- contVar
  l <- contVar
  y <- var
  expr' <- d_expr expr $ \z -> return (CA.AppCont l z)
  next <- k []
  return $
    CA.LetCont [CA.ContDef j [CA.Var x] next] $
      CA.LetVal y (CA.Lam $ CA.Lambda l [CA.Var x] expr') $
        CA.App (CA.Var "#fix") j [y]

d_decl (_ :< FnStmt fn) k = do
  decl <- d_fn fn
  CA.LetFun [decl] <$> k [CA.Var $ name $ getNode fn]

d_decl (_ :< enum@(Enum _ _ _)) k = do
  addEnum enum
  k []

d_decl (_ :< TypeAlias { aliasName, resolvedType = Just ty }) k =
  let x = CA.Var aliasName
   in CA.LetVal x (CA.Type ty) <$> k []

d_decl (m :< Operator _ _ opGenerics opLhs opName opRhs opRetType opBody) k =
  d_decl (m :< (FnStmt $ m :< Function { name = opName
                                       , generics = opGenerics
                                       , params = [opLhs, opRhs]
                                       , retType = opRetType
                                       , body = opBody
                                       })) k

d_decl (_ :< Class name _ methods) k = do
  j <- contVar
  x <- var
  methods' <- mapM d_fn methods
  let ctor = CA.FunDef (CA.Var name) j [x] (CA.AppCont j [x])
  CA.LetFun (ctor : methods') <$> k []

d_decl (_ :< Interface _ _ methods) k = do
  next <- k []
  foldM (flip d_intfMethod) next methods

d_decl (_ :< Implementation (name, _) generics ty methods) k =
  case mkConstraints generics of
    [] ->
      d_implItems methods $ \dict ->
        CA.LetVal dictVar dict <$> k []
    constr -> do
      j <- contVar
      x <- var
      items <- d_implItems methods $ \dict ->
        return $ CA.LetVal x dict (CA.AppCont j [x])
      CA.LetFun [CA.FunDef dictVar j constr items] <$> k []

  where
    dictVar = mkDictVar name ty

mkDictVar :: String -> U.Type -> CA.Var
mkDictVar name ty =
  CA.Var $ "%" ++ name ++ print ty
  where
    print (U.TApp ty _) = print ty
    print ty = show ty

mkDictVar' :: String -> Type -> CA.Var
mkDictVar' name ty =
  CA.Var $ "%" ++ name ++ print ty
  where
    print (TyApp ty _) = print ty
    print ty = show ty

d_fn :: Function -> DsM CA.FunDef
d_fn (m :< fn@(Function { params=[] })) =
  d_fn (m :< fn { params = [ignore] })

d_fn (_ :< fn) = do
  let params' = map (CA.Var . fst) (params fn)
  let f = CA.Var $ name fn
  j <- contVar
  body' <- d_codeBlock (body fn) $ \x -> return (CA.AppCont j x)
  let constrs = mkConstraints (generics fn)
  return $ CA.FunDef f j (constrs ++ params') body'

mkConstraints :: Generics -> [CA.Var]
mkConstraints gen =
  concatMap aux gen
  where
    aux (varName, bounds) =
      map (\bound -> mkDictVar bound (U.TName varName)) bounds ++ [CA.Var varName]

mk_var :: String -> CA.Var
mk_var v = CA.Var v

d_intfMethod :: InterfaceItem -> CA.Term -> DsM CA.Term
d_intfMethod (IntfVar (name,  _)) next = do
  s_name <- var
  j <- contVar
  let def = CA.FunDef (CA.Var name) j [CA.Var "#dict", CA.Var "#_"] (CA.LetVal s_name (CA.Lit $ String name) (CA.App (mk_var "#fieldAccess") j [s_name, mk_var "#dict"]))
  return $ CA.LetFun [def] next

d_intfMethod (IntfOperator { intfOpName }) next = do
  s_name <- var
  j <- contVar
  let def = CA.FunDef (CA.Var intfOpName) j [CA.Var "#dict", CA.Var "#_"] (CA.LetVal s_name (CA.Lit $ String intfOpName) (CA.App (mk_var "#fieldAccess") j [s_name, mk_var "#dict"]))
  return $ CA.LetFun [def] next

d_implItems :: [ImplementationItem] -> (CA.Value -> DsM CA.Term) -> DsM CA.Term
d_implItems items f = do
  let g k item =
        (\x -> d_implItem item (\y -> k $ y : x))
  let init fields =
        f (CA.Record fields)
  foldl g init items $ []

d_implItem :: ImplementationItem -> ((String, CA.Var) -> DsM CA.Term) -> DsM CA.Term
d_implItem (_ :< ImplVar (name, expr)) k =
  d_expr expr $ \[x] -> k (name, x)

d_implItem (_ :< fn@(ImplFunction { implName })) k = do
  j <- contVar
  body <- d_codeBlock (implBody fn) $ \x -> return $ CA.AppCont j x
  let f = CA.Var ("#" ++ implName)
  let def = CA.FunDef f j (map CA.Var (implParams fn)) body
  CA.LetFun [def] <$> k (implName, f)

d_implItem (_ :< op@(ImplOperator { implOpName })) k = do
  j <- contVar
  body <- d_codeBlock (implOpBody op) $ \x -> return $ CA.AppCont j x
  let name = CA.Var ("#" ++ implOpName)
  let def = CA.FunDef name j [CA.Var (implOpLhs op), CA.Var (implOpRhs op)] body
  CA.LetFun [def] <$> k (implOpName, name)

data Constraint
  = CHole
  | CType Type
  | CDict Type Intf
  | CApp Constraint [Constraint]

d_expr :: Expr -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_expr (_ :< VoidExpr) k = do
  x <- var
  CA.LetVal x CA.Unit <$> k [x]

--
d_expr (_ :< TypeCall callee constraints) k = do
  j <- contVar
  l <- contVar
  x <- var
  callee' <- d_expr callee (\x -> return $ CA.AppCont j x)
  computeConstraints constraints $ \(constraints', constraintHoles) -> do
    let lambda = CA.Lam $ CA.Lambda l constraintHoles (CA.App x l (reverse constraints'))
    contDef <- CA.ContDef j [x] <$> CA.LetVal x lambda <$> k [x]
    return $ CA.LetCont [contDef] callee'

d_expr (_ :< Literal l) k = do
  x <- var
  CA.LetVal x (CA.Lit l) <$> k [x]

d_expr (_ :< Ident ids) k =
  k [CA.Var $ last ids]

d_expr (_ :< ParenthesizedExpr expr) k =
  d_expr expr k

d_expr (m :< BinOp constrArgs tyArgs lhs (name, _) rhs) k =
  d_expr (m :< Call (m :< Ident [name]) constrArgs tyArgs [lhs, rhs]) k

d_expr (m :< Call callee constraints types []) k =
  d_expr (m :< Call callee constraints types [dummySpan :< VoidExpr]) k

d_expr (_ :< Call callee constraints _ args) k =
  computeConstraints constraints $ \(constraints', constraintHoles) -> do
    j <- contVar
    x <- var

    let
        f :: ([CA.Var] -> DsM CA.Term) -> Expr -> ([CA.Var] -> DsM CA.Term)
        f k arg = \args' -> d_expr arg (\arg' -> k $ args' ++ arg')

        init :: [CA.Var] -> DsM CA.Term
        init (callee':args')
          | constraintHoles == [] = do
            cont <- CA.ContDef j [x] <$> (k [x])
            return $ CA.LetCont [cont] app
          | otherwise =
            let lambda = CA.Lam (CA.Lambda j constraintHoles app)
             in CA.LetVal x lambda <$> k [x]
          where app = CA.App callee' j (reverse constraints' ++ args')

    d_expr callee $ \x -> foldl f init (reverse args) x

d_expr (_ :< Match expr cases) k = do
  j <- contVar
  x <- var
  cases' <- mapM (d_case j) cases
  expr' <- d_expr expr $ \x ->
    match x cases' CA.Error
  def <- CA.ContDef j [x] <$> k [x]
  return $ CA.LetCont [def] expr'
  where
    d_case j (_ :< Case (_ :< pat) body) = do
      body' <- d_codeBlock body $ \y ->
        return $ CA.AppCont j y
      return ([pat], body')

d_expr (_ :< Record fields) k = do
  x <- var
  let
      f k (id, expr) =
        \fields' ->  d_expr expr $ \[x] -> k $ (id, x) : fields'

      init fields' =
        CA.LetVal x (CA.Record fields') <$> k [x]
  foldl f init fields []

d_expr (_ :< FieldAccess expr field) k = do
  x <- var
  z <- var
  j <- contVar
  k' <- k [z]
  d_expr expr $ \[y] ->
    return $ CA.LetCont [CA.ContDef j [z] k'] $
      CA.LetVal x (CA.Lit $ String field) $
          CA.App (CA.Var "#fieldAccess") j [x, y]

d_expr (m :< If ifCond ifBody elseBody) k =
  d_expr match k
    where
      match = m :< Match { expr = ifCond
                         , cases = [ dummySpan :< Case { pattern = dummySpan :< PatCtor ("True", bool) []
                                                      , caseBody = ifBody
                                                }
                                   , dummySpan :< Case { pattern = dummySpan :< PatCtor ("False", bool) []
                                                      , caseBody = elseBody
                                                }
                                   ]
                         }

d_expr (_ :< List (Just ty) items) k =
  aux items (\x -> k [x])
    where
      aux :: [Expr] -> (CA.Var -> DsM CA.Term) -> DsM CA.Term
      aux [] k =
        k (CA.Var "Nil")

      aux (x:xs) k =
        d_expr x $ \[x'] ->
          aux xs $ \xs' ->
            cons x' xs' k


      cons :: CA.Var -> CA.Var -> (CA.Var -> DsM CA.Term) -> DsM CA.Term
      cons head tail k = do
        x <- var
        t <- var
        j <- contVar
        def <- CA.ContDef j [x] <$> k x
        return $
          CA.LetCont [def] $
            CA.LetVal t (CA.Type ty) $
              CA.App (CA.Var "Cons") j [t, head, tail]

d_expr (_ :< FnExpr fn) k = do
  -- replace the name of the function with a fresh variable
  --  to avoid shadowing as the expressions should not add
  --  values to the scope
  x <- var
  (CA.FunDef y j ps e) <- d_fn fn
  CA.LetFun [CA.FunDef x j ps (CA.subst e x y)] <$> k [x]

d_expr (_ :< Negate constrArgs expr) k =
  computeConstraints constrArgs $ \(constraints', _) ->
    d_expr expr $ \x -> do
      y <- var
      j <- contVar
      k' <- k [y]
      return $
        CA.LetCont [CA.ContDef j [y] k'] $
          CA.App (CA.Var "Std.negate") j (reverse constraints' ++ x)

-- TODO: figure out ordering and remove reverse usages all over
computeConstraints :: [ConstraintArg] -> (([CA.Var], [CA.Var]) -> DsM CA.Term) -> DsM CA.Term
computeConstraints cs k =
  foldl aux k (concatMap mkConstraint cs) ([], [])
  where
    aux :: (([CA.Var], [CA.Var]) -> DsM CA.Term) -> Constraint -> ([CA.Var], [CA.Var]) -> DsM CA.Term
    aux k CHole (args, holes) =
      let holeName = CA.Var $ "#hole" ++ show (length holes)
       in k (args ++ [holeName], holeName : holes)

    aux k (CType typeArg) (args, holes) = do
      x <- var
      CA.LetVal x (CA.Type typeArg) <$> k (args ++ [x], holes)

    aux k (CDict typeArg typeBound) (args, holes) =
      let constr = mkDictVar' (show typeBound) typeArg
       in k (args ++ [constr], holes)

    aux k (CApp typeArg nestedArgs) (args, holes) =
      foldl aux init nestedArgs ([], holes)
        where init (nestedArgs', holes') =
                aux j typeArg ([], holes')
                  where j t =
                          case t of
                            ([typeArg'], holes'') -> do
                              x <- var
                              j <- contVar
                              k' <- k (args ++ [x], holes'')
                              return $
                                CA.LetCont [CA.ContDef j [x] k'] $
                                  CA.App typeArg' j (reverse nestedArgs')
                            _ -> undefined

    mkConstraint :: ConstraintArg -> [Constraint]
    mkConstraint (CAType typeArg) =
      [mkTypeArg typeArg]

    mkConstraint (CABound typeArg typeBound) =
      [mkTypeBound (typeArg, typeBound), mkTypeArg typeArg]

    mkConstraint (CAPoly typeArg typeBound args) =
      let typeBound' = mkTypeBound (typeArg, typeBound)
          args' = concatMap mkConstraint args
       in [CApp typeBound' args', mkTypeArg typeArg]

    mkTypeArg :: Type -> Constraint
    mkTypeArg t | isHole t = CHole
    mkTypeArg t = CType t

    mkTypeBound :: (Type, Intf) -> Constraint
    mkTypeBound (typeArg, _) | isHole typeArg = CHole
    mkTypeBound (typeArg, typeBound) = CDict typeArg typeBound

ignore :: U.Param
ignore = ("#ignore", U.TPlaceholder)
