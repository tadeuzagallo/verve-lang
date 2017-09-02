module Desugar
  ( desugar
  , desugarStmt
  ) where

import Absyn
import Typing.Types
import qualified CoreAbsyn as CA

desugar :: Module (Id Type) Type -> CA.Expr
desugar = d_stmts . stmts

desugarStmt :: Stmt (Id Type) Type -> CA.Expr
desugarStmt stmt = d_stmts [stmt]

d_stmts :: [Stmt (Id Type) Type] -> CA.Expr
d_stmts [] = CA.Void
-- terminators
d_stmts ([Expr e]) =
  d_expr e
d_stmts ([FnStmt fn]) =
  let fn' = d_fn fn
   in CA.Let [(name fn, fn')] (CA.Var $ name fn)
d_stmts ([Let var expr]) =
  CA.Let [(var, d_expr expr)] (CA.Var var)

-- intermediaries
d_stmts (Let var expr : ss) =
  CA.Let [(var, d_expr expr)] (d_stmts ss)
d_stmts (Expr e:ss) =
  CA.Let [(ignore void, d_expr e)] (d_stmts ss)
d_stmts (FnStmt fn:ss) =
  CA.Let [(name fn, d_fn fn)] (d_stmts ss)
d_stmts (Enum name _ _ : ss) =
  CA.Let [(ignore Type, CA.Var name)] (d_stmts ss)
d_stmts (Operator _ _ opGenerics opLhs opName opRhs opRetType opBody : ss) =
  let fn = d_fn (Function { name = opName
                          , generics = opGenerics
                          , params = [opLhs, opRhs]
                          , retType = opRetType
                          , body = opBody
                          })
   in CA.Let [(opName, fn)] (d_stmts ss)
d_stmts (Class _ _ methods : ss) =
  let methods' = map (\fn -> (name fn, d_fn fn)) methods
   in CA.Let methods' (d_stmts ss)

d_stmts (Interface _ _ methods : ss) =
  CA.Let (map d_intfMethod methods) (d_stmts ss)

d_stmts (Implementation (name, _) generics ty methods : ss) =
  let dict = CA.Record (map (d_implMethod generics) methods)
      dictName = ("#" ++ name ++ print ty, void)
      fixedDict = CA.App (mk_var "#fix") (CA.Lam dictName dict)
   in CA.Let [(dictName, fixedDict)] (d_stmts ss)
  where
    print (TyApp ty _) = print ty
    print ty = show ty

d_fnNoFix :: Function (Id Type) Type -> CA.Expr
d_fnNoFix fn@(Function { params=[] }) =
  d_fn (fn { params = [ignore void] })

d_fnNoFix fn =
  let fn' = foldr CA.Lam (d_stmts $ body fn) (map (uncurry (,)) $ params fn)
   in foldr CA.Lam fn' (concatMap constraints $ generics fn)
  where
    constraints (varName, bounds) =
      map (\bound -> ("#" ++ show bound ++ varName, void)) bounds ++ [(varName, Type)]

d_fn :: Function (Id Type) Type -> CA.Expr
d_fn fn =
  let fn' = d_fnNoFix fn
   in CA.App (CA.Var ("#fix", void)) (CA.Lam (name fn) fn')

mk_var :: String -> CA.Expr
mk_var v = CA.Var (v, void)

d_intfMethod :: FunctionDecl (Id Type) Type -> CA.Bind
d_intfMethod (FunctionDecl name@(s_name, _) _ _ _) =
  let select = CA.App (mk_var "#fieldAccess") (CA.Lit (String s_name))
      select' = CA.App select (mk_var "#dict")
   in (name, CA.Lam ("#dict", void) (CA.Lam (ignore Type) select'))

d_implMethod :: [(Name, [Type])] -> Function (Id Type) Type -> CA.Bind
d_implMethod gen fn =
  let fn' = fn { generics = gen ++ generics fn }
   in (name fn, d_fnNoFix fn')

d_expr :: Expr (Id Type) Type -> CA.Expr
d_expr VoidExpr = CA.Void
d_expr (Literal l) = CA.Lit l
d_expr (Ident id) = CA.Var id
d_expr (ParenthesizedExpr expr) = d_expr expr
d_expr (Call callee constraints types []) = d_expr (Call callee constraints types [VoidExpr])
d_expr (BinOp constrArgs tyArgs lhs op rhs) =
  d_expr (Call (Ident op) constrArgs tyArgs [lhs, rhs])

d_expr (Call callee constraints types args) =
  let (constraints', constraintHoles) = foldl mkConstraint ([], []) constraints
      app = foldl CA.App (d_expr callee) constraints'
      (app', holes) = foldl mkTypeApp (app, []) types
      app'' = foldl mkApp app' args
      app''' = foldl (flip CA.Lam) app'' holes
   in foldl (flip CA.Lam) app''' constraintHoles
    where
      mkApp :: CA.Expr -> Expr (Id Type) Type -> CA.Expr
      mkApp callee arg =
        CA.App callee (d_expr arg)

      mkTypeApp :: (CA.Expr, [Id Type]) -> Type -> (CA.Expr, [Id Type])
      mkTypeApp (app, holes) ty | isHole ty =
        let holeName = "#hole" ++ show (length holes)
         in (CA.App app $ mk_var holeName, (holeName, void) : holes)
      mkTypeApp (app, holes) ty =
        (CA.App app $ CA.Type ty, holes)

      mkConstraint :: ([CA.Expr], [Id Type]) -> (Type, Type) -> ([CA.Expr], [Id Type])
      mkConstraint (constraints, holes) (typeArg, _) | isHole typeArg =
        let holeName = "#constr_hole" ++ show (length holes)
         in (constraints ++ [mk_var holeName], (holeName, void) : holes)
      mkConstraint (constraints, holes) (typeArg, Type) =
        (constraints ++ [CA.Type typeArg], holes)
      mkConstraint (constraints, holes) (typeArg, typeBound) =
        let constr = mk_var ("#" ++ show typeBound ++ show typeArg)
         in (constraints ++ [constr], holes)

d_expr (Match expr cases) = CA.Match (d_expr expr) (map d_case cases)

d_expr (Record fields) =
  let fields' = map (\(a, b) -> (a, d_expr b)) fields
   in CA.Record fields'

d_expr (FieldAccess expr ty (field, _)) =
  let dexpr = (d_expr expr)
      expr' =
        case ty of
          Cls _ _ -> CA.App (CA.Var ("#unwrapClass", void)) dexpr
          Rec _ -> dexpr
          _ -> undefined
   in CA.App
    (CA.App (CA.Var ("#fieldAccess", void)) (CA.Lit $ String field))
    expr'

d_expr (If ifCond ifBody elseBody) =
  CA.Match (d_expr ifCond) [ (CA.PatCtor ("True", bool) [], d_stmts ifBody)
                           , (CA.PatCtor ("False", bool) [], d_stmts elseBody)
                           ]

d_expr (List items) =
  aux items
    where
      aux [] = nil
      aux (x:xs) = cons (d_expr x) (aux xs)
      nil = CA.Var ("Nil", void)
      cons head tail = CA.App (CA.App (CA.Var ("Cons", void)) head) tail

d_expr (FnExpr fn) =
  d_fn fn

d_case :: Case (Id Type) Type -> CA.Case
d_case (Case pattern expr) = (d_pattern pattern, d_stmts expr)

d_pattern :: Pattern (Id Type) -> CA.Pattern
d_pattern PatDefault = CA.PatDefault
d_pattern (PatLiteral l) = CA.PatLiteral l
d_pattern (PatVar v) = CA.PatVar v
d_pattern (PatCtor name pats) = CA.PatCtor name (map d_pattern pats)

ignore :: Type -> Id Type
ignore ty = ("#ignore", ty)
