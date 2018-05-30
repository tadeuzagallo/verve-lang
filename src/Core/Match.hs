{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}
module Core.Match (match) where

import qualified Core.Absyn as CA
import Core.State
import Core.Desugar

import Absyn.Typed
import Typing.Types

import Data.List (groupBy, nub)
import Data.Foldable (foldrM)
import Data.Maybe (maybe)

{-
A CPS version of Wadler's pattern matching compiler from
`The Implementation of Functional Programming Languages`
-}

type Equation = ([ASTNode BasePattern Id SourceSpan], CA.Term)

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
  match vs [(ps, CA.subst e v (CA.Var x)) | (PatVar x : ps, e) <- eqs ] def

-- poor implementation, need a tuple or something better - maybe support records in core
--
-- NOTE: for future me trying to decipher this again:
--  1) we extract all the fields we are matching against to a list of variables
--  2) we extract all the patterns from the record append to list of matches to evaluated
--  3) we call match with the new variables and patterns
--
-- EXAMPLE:
--  match r { case {x : 1, y: 2}: f }
--
--  compiles to
--
--  let a = r.x
--  let b = r.y
--
--  match a {
--    case 1:
--      match b {
--        case 2: f
--      }
--  }
matchRecord :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchRecord ((CA.Var v') : vs) eqs def =
  foldl d_field match' fields []
    where
      match' vs' = match (vs' ++ vs) eqs' def
      eqs' = [ ([maybe undefined getNode (lookup field f) | field <- fields] ++ ps, e)  | (PatRecord f : ps, e) <- eqs ]
      fields = nub . concat $ [map fst f | (PatRecord f : _, _) <- eqs ]
      d_field k field =
        \x -> d_expr (dummySpan :< FieldAccess (dummySpan :< Ident [v']) field) $ \y -> k (y ++ x)

matchList :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
matchList vs eqs =
  match vs eqs'
    where
      eqs' = map desugar eqs

      desugar :: Equation -> Equation
      desugar (p : ps, e) = (desugarCons p : ps, e)

      desugarCons :: ASTNode BasePattern Id SourceSpan -> ASTNode BasePattern Id SourceSpan
      desugarCons (PatList ps rest) =
        foldl (\rest p -> PatCtor ("Cons", void) [p, dummySpan :< rest]) (desugarRest rest) ps

      desugarRest :: PatternRest -> ASTNode BasePattern Id SourceSpan
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
      eqs' = [ (map getNode ps' ++ ps, e) | (PatCtor _ ps' : ps, e) <- eqs]

dummySpan :: SourceSpan
dummySpan = SourceSpan { spanStart = error "unknown span start"
                       , spanEnd = error "unknown span end"
                       }
