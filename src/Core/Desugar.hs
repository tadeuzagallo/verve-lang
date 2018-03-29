{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Core.Desugar
  ( DsState
  , initialState
  , desugarStmt
  , desugarStmts
  , d_expr
  ) where

import Core.State
import {-# SOURCE #-} Core.Match

import Absyn.Typed
import Typing.Types
import qualified Core.Absyn as CA

import Control.Monad (foldM)
import Data.List (groupBy)

desugarStmts :: DsState -> [Stmt] -> (DsState, CA.Term)
desugarStmts state stmts =
  runDesugar (d_stmts stmts) state

desugarStmt :: DsState -> Stmt -> (DsState, CA.Term)
desugarStmt s =
  desugarStmts s . (:[])

d_stmts :: [Stmt] -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_stmts stmts k =
  let stmts' = groupBy f stmts
   in g stmts' k
    where
      f (Decl _) (Decl _) = True
      f (Expr _) (Expr _) = True
      f _ _ = False

      g (ds@(Decl _: _) : rest) k = do
        let r =
              case rest of
                [] -> k
                r -> \_ -> g r k
        d_decls (map (\(Decl d) -> d) ds) r

      g (es@(Expr _ : _) : rest) k = do
        let init = case rest of
                     [] -> \x -> k x
                     r -> \_ -> g r k
        foldr (\e k _ -> d_expr e k) init [e | Expr e <- es] []

      g [] k =
        d_expr VoidExpr k


d_decls :: [Decl] -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_decls [d] k =
  d_decl d k

d_decls (d:ds) k = do
  d_decl d (\_ -> d_decls ds k)

d_decl :: Decl -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_decl (Let (x, _) expr) k = do
  j <- contVar
  l <- contVar
  y <- var
  expr' <- d_expr expr $ \z -> return (CA.AppCont l z)
  next <- k []
  return $
    CA.LetCont [CA.ContDef j [CA.Var x] next] $
      CA.LetVal y (CA.Lam $ CA.Lambda l [CA.Var x] expr') $
        CA.App (CA.Var "#fix") j [y]

d_decl (FnStmt fn) k = do
  decl <- d_fn fn
  CA.LetFun [decl] <$> k [CA.Var $ fst $ name fn]

d_decl enum@(Enum _ _ _) k = do
  addEnum enum
  k []

d_decl (TypeAlias { aliasName, aliasType }) k =
  let x = CA.Var aliasName
   in CA.LetVal x (CA.Type aliasType) <$> k []

d_decl (Operator _ _ opGenerics opLhs opName opRhs opRetType opBody) k =
  d_decl (FnStmt $ Function { name = opName
                            , generics = opGenerics
                            , params = [opLhs, opRhs]
                            , retType = opRetType
                            , body = opBody
                            }) k

d_decl (Class _ _ methods) k =
  CA.LetFun <$> mapM d_fn methods <*> k []

d_decl (Interface _ _ methods) k = do
  next <- k []
  foldM (flip d_intfMethod) next methods

d_decl (Implementation (name, _) generics ty methods) k =
  case mkConstraints generics of
    [] ->
      d_implItems methods $ \dict ->
        CA.LetVal (CA.Var dictName) dict <$> k []
    constr -> do
      j <- contVar
      x <- var
      items <- d_implItems methods $ \dict ->
        return $ CA.LetVal x dict (CA.AppCont j [x])
      CA.LetFun [CA.FunDef (CA.Var dictName)  j constr items] <$> k []

  where
    dictName = "#" ++ name ++ print ty

    print (TyApp ty _) = print ty
    print ty = show ty

d_fn :: Function -> DsM CA.FunDef
d_fn fn@(Function { params=[] }) =
  d_fn (fn { params = [ignore void] })

d_fn fn = do
  let params' = map (CA.Var . fst) (params fn)
  let f = CA.Var $ fst $ name fn
  j <- contVar
  body' <- d_stmts (body fn) $ \x -> return (CA.AppCont j x)
  let constrs = mkConstraints (generics fn)
  return $ CA.FunDef f j (constrs ++ params') body'

mkConstraints :: Generics -> [CA.Var]
mkConstraints gen =
  concatMap aux gen
  where
    aux (varName, bounds) =
      map (\bound -> CA.Var $ "#" ++ show bound ++ varName) bounds ++ [CA.Var varName]

mk_var :: String -> CA.Var
mk_var v = CA.Var v

d_intfMethod :: InterfaceItem -> CA.Term -> DsM CA.Term
d_intfMethod (IntfVar (name,  _)) next = do
  s_name <- var
  j <- contVar
  let def = CA.FunDef (CA.Var name) j [CA.Var "#dict", CA.Var "#_"] (CA.LetVal s_name (CA.Lit $ String name) (CA.App (mk_var "#fieldAccess") j [s_name, mk_var "#dict"]))
  return $ CA.LetFun [def] next

d_intfMethod (IntfOperator { intfOpName = (name,  _) }) next = do
  s_name <- var
  j <- contVar
  let def = CA.FunDef (CA.Var name) j [CA.Var "#dict", CA.Var "#_"] (CA.LetVal s_name (CA.Lit $ String name) (CA.App (mk_var "#fieldAccess") j [s_name, mk_var "#dict"]))
  return $ CA.LetFun [def] next

d_implItems :: [ImplementationItem] -> (CA.Value -> DsM CA.Term) -> DsM CA.Term
d_implItems items f = do
  let g k item =
        (\x -> d_implItem item (\y -> k $ y : x))
  let init fields =
        f (CA.Record fields)
  foldl g init items $ []

d_implItem :: ImplementationItem -> ((Id, CA.Var) -> DsM CA.Term) -> DsM CA.Term
d_implItem (ImplVar (name, expr)) k =
  d_expr expr $ \[x] -> k (name, x)

d_implItem fn@(ImplFunction { implName=(name, _) }) k = do
  j <- contVar
  body <- d_stmts (implBody fn) $ \x -> return $ CA.AppCont j x
  let f = CA.Var ("#" ++ name)
  let def = CA.FunDef f j (map CA.Var (implParams fn)) body
  CA.LetFun [def] <$> k (implName fn, f)

d_implItem op@(ImplOperator {}) k = do
  j <- contVar
  body <- d_stmts (implOpBody op) $ \x -> return $ CA.AppCont j x
  let name = CA.Var ("#" ++ (fst $ implOpName op))
  let def = CA.FunDef name j [CA.Var (implOpLhs op), CA.Var (implOpRhs op)] body
  CA.LetFun [def] <$> k (implOpName op, name)

data Constraint
  = CHole
  | CType Type
  | CDict Type Intf
  | CApp Constraint [Constraint]

d_expr :: Expr -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_expr VoidExpr k = do
  x <- var
  CA.LetVal x CA.Unit <$> k [x]

--
d_expr (TypeCall callee constraints) k = do
  j <- contVar
  l <- contVar
  x <- var
  callee' <- d_expr callee (\x -> return $ CA.AppCont j x)
  computeConstraints constraints $ \(constraints', constraintHoles) -> do
    let lambda = CA.Lam $ CA.Lambda l constraintHoles (CA.App x l (reverse constraints'))
    contDef <- CA.ContDef j [x] <$> CA.LetVal x lambda <$> k [x]
    return $ CA.LetCont [contDef] callee'

d_expr (Literal l) k = do
  x <- var
  CA.LetVal x (CA.Lit l) <$> k [x]

d_expr (Ident ids _) k =
  k [CA.Var $ last ids]

d_expr (ParenthesizedExpr expr) k =
  d_expr expr k

d_expr (BinOp constrArgs tyArgs lhs (name, ty) rhs) k =
  d_expr (Call (Ident [name] ty) constrArgs tyArgs [lhs, rhs]) k

d_expr (Call callee constraints types []) k =
  d_expr (Call callee constraints types [VoidExpr]) k

d_expr (Call callee constraints _ args) k =
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

d_expr (Match expr cases) k = do
  j <- contVar
  x <- var
  cases' <- mapM (d_case j) cases
  expr' <- d_expr expr $ \x ->
    match x cases' CA.Error
  def <- CA.ContDef j [x] <$> k [x]
  return $ CA.LetCont [def] expr'
  where
    d_case j (Case pat body) = do
      body' <- d_stmts body $ \y ->
        return $ CA.AppCont j y
      return ([pat], body')

d_expr (Record fields) k = do
  x <- var
  let
      f k (id, expr) =
        \fields' ->  d_expr expr $ \[x] -> k $ (id, x) : fields'

      init fields' =
        CA.LetVal x (CA.Record fields') <$> k [x]
  foldl f init fields []

d_expr (FieldAccess expr ty (field, _)) k = do
  x <- var
  z <- var
  j <- contVar
  l <- contVar
  k' <- k [z]
  let
    unwrapIfNecessary y k =
      case ty of
        Cls _ ->
          CA.LetCont [CA.ContDef l [z] (k z)] $
            CA.App (CA.Var "#unwrapClass") l [y]
        Rec _ ->
          k y
        _ -> undefined
  d_expr expr $ \[y] ->
    return $ CA.LetCont [CA.ContDef j [z] k'] $
      CA.LetVal x (CA.Lit $ String field) $
        unwrapIfNecessary y $ \y' ->
          CA.App (CA.Var "#fieldAccess") j [x, y']

d_expr (If ifCond ifBody elseBody) k =
  d_expr match k
    where
      match = Match { expr = ifCond
                    , cases = [ Case { pattern = PatCtor ("True", bool) []
                                     , caseBody = ifBody
                                     }
                              , Case { pattern = PatCtor ("False", bool) []
                                     , caseBody = elseBody
                                     }
                              ]
                    }

d_expr (List ty items) k =
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

d_expr (FnExpr fn) k = do
  decl <- d_fn fn
  CA.LetFun [decl] <$> k [CA.Var $ fst $ name fn]

d_expr (Negate constrArgs expr) k =
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
      let constr = mk_var ("#" ++ show typeBound ++ show typeArg)
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

ignore :: Type -> Id
ignore ty = ("#ignore", ty)
