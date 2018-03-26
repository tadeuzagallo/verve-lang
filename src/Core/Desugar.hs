{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Core.Desugar
  ( DsState
  , initialState
  , desugarStmt
  , desugarStmts
  ) where

import Absyn.Typed
import Typing.Types
import qualified Core.Absyn as CA

import Control.Monad (foldM)
import Control.Monad.State (State, runState, gets, modify)
import Data.Foldable (foldrM)
import Data.List (groupBy, nub)
import Data.Maybe (maybe)
import Data.Tuple (swap)

desugarStmts :: DsState -> [Stmt] -> (DsState, CA.Term)
desugarStmts state stmts =
  let k z = return (CA.AppCont (CA.ContVar "halt") z)
   in swap $ runState (d_stmts stmts k) state

desugarStmt :: DsState -> Stmt -> (DsState, CA.Term)
desugarStmt s = desugarStmts s . (:[])


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
{-d_stmts [] k =-}
  {-d_expr VoidExpr k-}

{-d_stmts [stmt] k =-}
{-d_stmts (stmt : stmts) k = do-}
  {-j <- contVar-}
  {-stmt' <- case stmt of-}
             {-Decl decl -> d_decl decl $ \x -> return (CA.AppCont j x)-}
             {-Expr expr -> d_expr expr $ \x -> return (CA.AppCont j x)-}
  {-stmts' <- d_stmts stmts k-}
  {-return $ CA.LetCont [CA.ContDef j [] stmts'] stmt'-}

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
  d_fn fn k

d_decl (Enum (name, _) _ ctors) k = do
  modify (addEnum $ DsEnum { enumName = name, enumCtors = ctors' })
  k []
    where
      ctors' = map mkCtor ctors
      mkCtor ((name, _), args) = DsCtor { ctorName = name, arity = maybe 0 length args }

d_decl (TypeAlias { aliasName, aliasType }) k =
  let x = CA.Var aliasName
   in CA.LetVal x (CA.Type aliasType) <$> k []

d_decl (Operator _ _ opGenerics opLhs opName opRhs opRetType opBody) k =
  d_fn (Function { name = opName
                 , generics = opGenerics
                 , params = [opLhs, opRhs]
                 , retType = opRetType
                 , body = opBody
                 }) k

d_decl (Class _ _ methods) k =
  let f k method =
        \_ -> d_fn method k
   in foldl f k methods []

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
      CA.LetVal (CA.Var dictName) (CA.Lam $ CA.Lambda j constr items) <$> k []

  where
    dictName = "#" ++ name ++ print ty

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

-- Pattern Matching compiler [Wadler, 1985]

type Equation = ([Pattern], CA.Term)

match :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
match [] [] def = return def
match [] (([], e) : _) _ = return e

match vs eqs def =
  foldrM (\eqs def -> match' vs eqs def) def g
    where
      g = groupBy h eqs
      h (PatDefault : _, _) (PatDefault : _, _) = True
      h (PatLiteral _ : _, _) (PatLiteral _ : _, _) = True
      h (PatVar _ : _, _) (PatVar _ : _, _) = True
      h (PatRecord _ : _, _) (PatRecord _ : _, _) = True
      h (PatList _ _ : _, _) (PatList _ _ : _, _) = True
      h (PatCtor _ _ : _, _) (PatCtor _ _ : _, _) = True
      h _ _ = False

match' :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
match' vs eqs@((PatDefault : _ , _): _) def =
  matchDefault vs eqs def

match' vs eqs@((PatLiteral _ : _ , _): _) def =
  matchLit vs eqs def

match' vs eqs@((PatVar _ : _ , _): _) def =
  matchVar vs eqs def

match' vs eqs@((PatRecord _ : _ , _): _) def =
  matchRecord vs eqs def

match' vs eqs@((PatList _ _ : _ , _): _) def =
  matchList vs eqs def

match' vs eqs@((PatCtor _ _ : _ , _): _) def =
  matchCtor vs eqs def

matchDefault :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchDefault (_ : vs) eqs def =
  match vs eqs' def
    where eqs' = [ (ps, e) | (_ : ps, e) <- eqs ]

matchLit :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchLit (v:vs) eqs def = do
  foldrM f def eqs'
    where
      eqs' = groupBy eqPatLit eqs

      eqPatLit (PatLiteral l : _, _) (PatLiteral l' : _, _) = l == l'

      f eqs@((PatLiteral p:_, _) : _) def' = do
        j <- contVar
        t <- contVar
        f <- contVar
        x <- var
        tBody <- match vs [(ps, e) | (_ : ps, e) <- eqs] def

        let defJ = CA.ContDef j [x] kase
            defT = CA.ContDef t [] tBody
            defF = CA.ContDef f [] def'
            kase = CA.Case x [ CA.Clause "True" t
                             , CA.Clause "False" f
                             ]
        return $
          CA.LetCont [defT, defF, defJ] $
          CA.LetVal x (CA.Lit p) $
          CA.App (CA.Var "#literalEquality") j [v, x]

matchVar :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchVar (v:vs) eqs def =
  match vs [(ps, CA.subst e v (CA.Var x)) | (PatVar (x, _) : ps, e) <- eqs ] def

-- poor implementation, need a tuple or something better - maybe support records in core
matchRecord :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchRecord ((CA.Var v') : vs) eqs def =
  foldl d_field match' fields []
    where
      match' vs' = match (vs' ++ vs) eqs' def
      eqs' = [ ([maybe PatDefault id (lookup field f) | field <- fields] ++ ps, e)  | (PatRecord f : ps, e) <- eqs ]
      fields = nub . concat $ [ map fst f | (PatRecord f : _, _) <- eqs ]
      d_field k field =
        \x -> d_expr (FieldAccess (Ident [v'] void) (Rec []) field) $ \y -> k (y ++ x)

matchList :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchList vs eqs =
  match vs eqs'
    where
      eqs' = map desugar eqs

      desugar :: Equation -> Equation
      desugar (p : ps, e) = (desugarCons p : ps, e)

      desugarCons :: Pattern -> Pattern
      desugarCons (PatList ps rest) =
        foldl (\rest p -> PatCtor ("Cons", void) [p, rest]) (desugarRest rest) ps

      desugarRest :: PatternRest -> Pattern
      desugarRest NoRest = PatCtor ("Nil", void) []
      desugarRest DiscardRest = PatDefault
      desugarRest (NamedRest a) = PatVar a

matchCtor :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchCtor (v:vs) eqs def = do
  enum <- findEnumForCtor (getCtor (head eqs))
  (defs, clauses) <- unzip <$> mapM (\c -> matchClause c vs (choose c eqs) def) (enumCtors enum)
  return $ CA.LetCont defs (CA.Case v clauses)

getCtor :: Equation -> String
getCtor (PatCtor c _: _, _) = fst c

choose :: DsCtor -> [Equation] -> [Equation]
choose c = filter (\eq -> getCtor eq == ctorName c)

matchClause :: DsCtor -> [CA.Var] -> [Equation] -> CA.Term -> DsM (CA.ContDef, CA.Clause)
matchClause c vs eqs def = do
  j <- contVar
  vs' <- mapM (const var) [1..n]
  e <- match (vs' ++ vs) eqs' def
  let def = CA.ContDef j vs' e
  let clause = CA.Clause (ctorName c) j
  return (def, clause)
    where
      n = arity c
      eqs' = [ (ps' ++ ps, e) | (PatCtor _ ps' : ps, e) <- eqs]
