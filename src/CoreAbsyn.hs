module CoreAbsyn where

import Absyn (Id, Literal)
import Typing.Types (Type)

data Expr
  = Void
  | Lit Literal
  | Var (Id Type)
  | App Expr Expr
  | Lam (Id Type) Expr
  | Let [Bind] Expr
  | Match Expr [Case]
  | Type Type
  | Record [Bind]

type Bind = (Id Type, Expr)

type Case = (Pattern, Expr)

instance Show Expr where
  show = showExpr Top

data Ctx = Top | Bot
showExpr :: Ctx -> Expr -> String
showExpr _ Void = "()"
showExpr _ (Lit l) = show l
showExpr _ (Var id) = showId id
showExpr _ (Type ty) = "@" ++ show ty

showExpr _ (Record fields) =
  let showField (id, expr) = "\t" ++ showId id ++ " = " ++ showExpr Top expr
   in "{\n" ++ unlines (map showField fields) ++ "}"

showExpr Top (App f arg) = showExpr Top f ++ " " ++ showExpr Bot arg

showExpr Top (Lam id body) = "λ" ++ showId id ++ ". " ++ showExpr Top body

showExpr Top (Let bind expr) =
  unlines (map showBind bind) ++ showExpr Top expr

showExpr Top (Match expr cases) =
  "case " ++ show expr ++ " of\n" ++ unlines (map showCase cases)

showExpr Bot expr = "(" ++ showExpr Top expr ++ ")"

showId :: Id Type -> String
showId (v, _) = v

showBind :: Bind -> String
showBind (id, expr) =
  "let " ++ showId id ++ " = " ++ show expr

showCase :: Case -> String
showCase (pat, expr) =
  "\t" ++ show pat ++ " -> " ++ show expr

data Pattern
  = PatDefault
  | PatLiteral Literal
  | PatVar (Id Type)
  | PatCtor (Id Type) [Pattern]

instance Show Pattern where
  show = showPat Top

showPat :: Ctx -> Pattern -> String
showPat _ PatDefault = "_"
showPat _ (PatLiteral l) = show l
showPat _ (PatVar id) = showId id
showPat Top (PatCtor id pats) = showId id ++ " " ++ unwords (reverse $ map (showPat Bot) pats)
showPat Bot pat@(PatCtor _ _) = "(" ++ showPat Top  pat ++ ")"
