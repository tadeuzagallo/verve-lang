{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Core.Desugar
  ( desugar
  , desugarStmt
  , desugarStmts
  ) where

import Absyn.Typed
import Typing.Types
import qualified Core.Absyn as CA

import Control.Monad (foldM)
import Control.Monad.State (State, evalState, gets, modify)
import Data.List (groupBy)
import Data.Maybe (maybe)

desugar :: Module -> CA.Term
desugar m =
  let k z = return (CA.AppCont (CA.ContVar "halt") z)
   in evalState (d_stmts (stmts m) k) initialState

desugarStmts :: [Stmt] -> CA.Term
desugarStmts stmts =
  let k z = return (CA.AppCont (CA.ContVar "halt") z)
   in evalState (d_stmts stmts k) initialState

desugarStmt :: Stmt -> CA.Term
desugarStmt = desugarStmts . (:[])


data DsEnum = DsEnum { enumName :: String, enumCtors :: [DsCtor] }
data DsCtor = DsCtor { ctorName :: String, arity :: Int }
data DsState = DsState { contCount :: Int, varCount :: Int, enums :: [DsEnum] }

initialState :: DsState
initialState = DsState
  { contCount = 0
  , varCount = 0
  -- TODO: this should live in the registry somehow
  , enums = [ DsEnum { enumName = "Bool"
                     , enumCtors = [ DsCtor "True" 0
                                   , DsCtor "False" 0
                                   ]
                     }
            , DsEnum { enumName = "List"
                     , enumCtors = [ DsCtor "Nil" 0
                                   , DsCtor "Cons" 2
                                   ]
                     }
            ]
  }

type DsM = State DsState

contVar :: DsM CA.ContVar
contVar = do
  count <- gets contCount
  modify $ \s -> s { contCount = count + 1 }
  return $ CA.ContVar ("#k" ++ show count)

var :: DsM CA.Var
var = do
  count <- gets varCount
  modify $ \s -> s { varCount = count + 1 }
  return $ CA.Var ("#x" ++ show count)

addEnum :: DsEnum -> DsState -> DsState
addEnum enum state =
  state { enums = enum : (enums state) }


findEnumForCtor :: String -> DsM DsEnum
findEnumForCtor c = do
  es <- gets enums
  return $ f es
    where
      f :: [DsEnum] -> DsEnum
      f (e:es) =
        if g (enumCtors e) then e else f es
      g :: [DsCtor] -> Bool
      g [] = False
      g (c' : cs)
        | ctorName c' == c = True
        | otherwise = g cs


d_stmts :: [Stmt] -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_stmts [] k =
  d_expr VoidExpr k

d_stmts [stmt] k =
  case stmt of
    Decl decl -> d_decl decl k
    Expr expr -> d_expr expr k

d_stmts (stmt : stmts) k = do
  j <- contVar
  stmt' <- case stmt of
             Decl decl -> d_decl decl $ \x -> return (CA.AppCont j x)
             Expr expr -> d_expr expr $ \x -> return (CA.AppCont j x)
  stmts' <- d_stmts stmts k
  return $ CA.LetCont [CA.ContDef j [] stmts'] stmt'


d_decl :: Decl -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_decl (Let (x, _) expr) k = do
  j <- contVar
  expr' <- d_expr expr $ \z -> return (CA.AppCont j z)
  x' <- k [CA.Var x]
  return $ CA.LetCont [CA.ContDef j [CA.Var x] x'] expr'

d_decl (FnStmt fn) k = do
  d_fn fn k

d_decl (Enum (name, _) _ ctors) k = do
  x <- var
  modify (addEnum $ DsEnum { enumName = name, enumCtors = ctors' })
  CA.LetVal x CA.Unit <$> k [x]
    where
      ctors' = map mkCtor ctors
      mkCtor ((name, _), args) = DsCtor { ctorName = name, arity = maybe 0 length args }

d_decl (TypeAlias { aliasName, aliasType }) k =
  let x = CA.Var aliasName
   in CA.LetVal x (CA.Type aliasType) <$> k [x]

d_decl (Operator _ _ opGenerics opLhs opName opRhs opRetType opBody) k =
  d_fn (Function { name = opName
                 , generics = opGenerics
                 , params = [opLhs, opRhs]
                 , retType = opRetType
                 , body = opBody
                 }) k

d_decl (Class _ _ methods) k =
  let f k method =
        (\_ -> d_fn method k)
      j = foldl f k methods
   in var >>= \y -> CA.LetVal y CA.Unit <$> j [y]

d_decl (Interface _ _ methods) k =
  let f k method =
        (\_ -> d_intfMethod method k)
      j = foldl f k methods
   in var >>= \y -> CA.LetVal y CA.Unit <$> j [y]

d_decl (Implementation (name, _) generics ty methods) k = do
  j <- contVar
  dict <- d_implItems methods $ \x -> return $ CA.AppCont j x
  let dictName = "#" ++ name ++ print ty
  let dictLam = CA.Lam $ CA.Lambda j (mkConstraints generics) dict
  CA.LetVal (CA.Var dictName) dictLam <$> k [CA.Var dictName]
  where
    print (TyApp ty _) = print ty
    print ty = show ty

d_fn :: Function -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_fn fn@(Function { params=[] }) k =
  d_fn (fn { params = [ignore void] }) k

d_fn fn k = do
  let params' = map (CA.Var . fst) (params fn)
  let f = CA.Var $ fst $ name fn
  j <- contVar
  body' <- d_stmts (body fn) $ \x -> return (CA.AppCont j x)
  let constrs = mkConstraints (generics fn)
  CA.LetFun [CA.FunDef f j (constrs ++ params') body'] <$> k [f]

mkConstraints :: Generics -> [CA.Var]
mkConstraints gen =
  concatMap aux gen
  where
    aux (varName, bounds) =
      map (\bound -> CA.Var $ "#" ++ show bound ++ varName) bounds ++ [CA.Var varName]

mk_var :: String -> CA.Var
mk_var v = CA.Var v

d_intfMethod :: InterfaceItem -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_intfMethod (IntfVar (name,  _)) k = do
  s_name <- var
  j <- contVar
  let def = CA.FunDef (CA.Var name) j [CA.Var "#dict", CA.Var "#_"] (CA.LetVal s_name (CA.Lit $ String name) (CA.App (mk_var "#fieldAccess") j [s_name, mk_var "#dict"]))
  CA.LetFun [def] <$> k [CA.Var name]

d_intfMethod (IntfOperator { intfOpName = (name,  _) }) k = do
  s_name <- var
  j <- contVar
  let def = CA.FunDef (CA.Var name) j [CA.Var "#dict", CA.Var "#_"] (CA.LetVal s_name (CA.Lit $ String name) (CA.App (mk_var "#fieldAccess") j [s_name, mk_var "#dict"]))
  CA.LetFun [def] <$> k [CA.Var name]

d_implItems :: [ImplementationItem] -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
d_implItems items k = do
  x <- var
  let f k item =
        (\x -> d_implItem item (\y -> k $ y : x))
  let init fields =
        CA.LetVal x (CA.Record fields) <$> k [x]
  foldl f init items $ []

d_implItem :: ImplementationItem -> ((Id, CA.Var) -> DsM CA.Term) -> DsM CA.Term
d_implItem (ImplVar (name, expr)) k =
  d_expr expr $ \[x] -> k (name, x)

d_implItem fn@(ImplFunction { implName=(name, _) }) k = do
  j <- contVar
  body <- d_stmts (implBody fn) $ \x -> return $ CA.AppCont j x
  let f = CA.Var name
  let def = CA.FunDef f j (map CA.Var (implParams fn)) body
  CA.LetFun [def] <$> k (implName fn, f)

d_implItem op@(ImplOperator {}) k = do
  j <- contVar
  body <- d_stmts (implOpBody op) $ \x -> return $ CA.AppCont j x
  let name = CA.Var (fst $ implOpName op)
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
    let lambda = CA.Lam $ CA.Lambda l constraintHoles (CA.App x l constraints')
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
        f k arg = \args' -> d_expr arg (\arg' ->
          k $ args' ++ arg')

        init :: [CA.Var] -> DsM CA.Term
        init (callee':args')
          | constraintHoles == [] = do
            cont <- CA.ContDef j [x] <$> (k [x])
            return $ CA.LetCont [cont] $ CA.App callee' j (constraints' ++ args')
          | otherwise =
              let app = CA.App callee' j (constraints' ++ args')
                  lambda = CA.Lam (CA.Lambda j constraintHoles app)
               in CA.LetVal x lambda <$> k [x]
    d_expr callee $ \x -> foldl f init (reverse args) x

d_expr (Match expr cases) k = do
  cases' <- mapM d_case cases
  d_expr expr $ \x ->
    match x cases' (error "pattern match failed") k
  where
    d_case (Case pat body) = do
      body' <- d_stmts body k
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

d_expr (FnExpr fn) k =
  d_fn fn k

d_expr (Negate constrArgs expr) k =
  computeConstraints constrArgs $ \(constraints', _) ->
    d_expr expr $ \x -> do
      y <- var
      j <- contVar
      k' <- k [y]
      return $
        CA.LetCont [CA.ContDef j [y] k'] $
          CA.App (CA.Var "Std.negate") j (constraints' ++ x)

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
                                  CA.App typeArg' j nestedArgs'
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


{-d_case :: Case -> (CA.ContVar -> DsM CA.Term) -> DsM CA.Term-}
{-d_case kase k =-}

{-d_pattern :: Pattern -> CA.Pattern-}
{-d_pattern PatDefault = CA.PatDefault-}
{-d_pattern (PatLiteral l) = CA.PatLiteral l-}
{-d_pattern (PatVar v) = CA.PatVar . CA.Var . fst $ v-}

{-d_pattern (PatRecord fields) =-}
  {-CA.PatRecord $ map (second d_pattern) fields-}

{-d_pattern (PatList pats rest) =-}
  {-let init = case rest of-}
               {-NoRest -> CA.PatCtor ("Nil", void) []-}
               {-DiscardRest -> CA.PatDefault-}
               {-NamedRest n -> CA.PatVar n-}
      {-aux pat tail =-}
        {-let pat' = d_pattern pat-}
         {-in CA.PatCtor ("Cons", void) [tail, pat']-}
   {-in foldr aux init (reverse pats)-}

{-d_pattern (PatCtor name pats) = CA.PatCtor name (map d_pattern pats)-}

ignore :: Type -> Id
ignore ty = ("#ignore", ty)

-- Pattern Matching compiler [Wadler, 1985]

type Equation = ([Pattern], CA.Term)

match :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
match [] eqs def _k =
  -- TODO: use double-barreled expressions to emulate fatbar
  foldM f def eqs
    where
      f _ ([], e) = return e

match vs eqs def k =
  foldM (\def eqs -> match' vs eqs def k) def g
    where
      g = groupBy h eqs
      h (PatDefault : _, _) (PatDefault : _, _) = True
      h (PatLiteral _ : _, _) (PatLiteral _ : _, _) = True
      h (PatVar _ : _, _) (PatVar _ : _, _) = True
      h (PatRecord _ : _, _) (PatRecord _ : _, _) = True
      h (PatList _ _ : _, _) (PatList _ _ : _, _) = True
      h (PatCtor _ _ : _, _) (PatCtor _ _ : _, _) = True
      h _ _ = False

match' :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
match' vs eqs@((PatDefault : _ , _): _) def k =
  matchDefault vs eqs def k

match' vs eqs@((PatLiteral _ : _ , _): _) def k =
  matchLit vs eqs def k

match' vs eqs@((PatVar _ : _ , _): _) def k =
  matchVar vs eqs def k

match' vs eqs@((PatRecord _ : _ , _): _) def k =
  matchRecord vs eqs def k

match' vs eqs@((PatList _ _ : _ , _): _) def k =
  matchList vs eqs def k

match' vs eqs@((PatCtor _ _ : _ , _): _) def k =
  matchCtor vs eqs def k

matchDefault :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
matchDefault (_ : vs) eqs def k =
  match vs eqs' def k
    where eqs' = [ (ps, e) | (_ : ps, e) <- eqs ]

matchLit :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
matchLit =
  -- TODO
  undefined
  {-[ \x -> if v == c then e else k x | (c : ps, e) <- eqs]-}

matchVar :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
matchVar (v:vs) eqs def k =
  match vs [(ps, CA.subst e v (CA.Var x)) | (PatVar (x, _) : ps, e) <- eqs ] def k

matchRecord :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
matchRecord =
  -- TODO
  undefined
{-let xi = r.i, ..., in-}
    {-match (x1, ..., xn) with (p1, ..., pn)-}

matchList :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
matchList =
  -- TODO
  undefined

matchCtor :: [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM CA.Term
matchCtor (v:vs) eqs def k = do
  enum <- findEnumForCtor (getCtor (head eqs))
  (defs, clauses) <- unzip <$> mapM (\c -> matchClause c vs (choose c eqs) def k) (enumCtors enum)
  return $ CA.LetCont defs (CA.Case v clauses)

getCtor :: Equation -> String
getCtor (PatCtor c _: _, _) = fst c

choose :: DsCtor -> [Equation] -> [Equation]
choose c = filter (\eq -> getCtor eq == ctorName c)

matchClause :: DsCtor -> [CA.Var] -> [Equation] -> CA.Term -> ([CA.Var] -> DsM CA.Term) -> DsM (CA.ContDef, CA.Clause)
matchClause c vs eqs def k = do
  j <- contVar
  vs' <- mapM (const var) [1..n]
  e <- match (vs' ++ vs) eqs' def k
  let def = CA.ContDef j vs' e
  let clause = CA.Clause (ctorName c) j
  return (def, clause)
    where
      n = arity c
      eqs' = [ (ps' ++ ps, e) | (PatCtor _ ps' : ps, e) <- eqs]
