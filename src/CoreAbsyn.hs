module CoreAbsyn where

import Absyn.Typed (Id, Literal)
import Typing.Types (Type)

import qualified Typing.Types as T (Type(Type))

import Data.List (intercalate)

data Expr
  = Void
  | Lit Literal
  | Var Id
  | App Expr Expr
  | Lam Id Expr
  | Let [Bind] Expr
  | Match Expr [Case]
  | Type Type
  | Record [Bind]

type Bind = (Id, Expr)

type Case = (Pattern, Expr)

instance Show Expr where
  show = printOut . showExpr Top

data Out
  = Str String
  | Indent
  | Dedent
  | LineBreak

printOut :: [Out] -> String
printOut out = printOutAux 0 out

printOutAux :: Int -> [Out] -> String
printOutAux _ [] = ""
printOutAux d (Str s : out) = s ++ " " ++ printOutAux d out
printOutAux d (Indent : out) = printOutAux (d + 1) out
printOutAux d (Dedent : out) = printOutAux (d - 1) out
printOutAux d (LineBreak : out) = "\n" ++ replicate (d * 2) ' ' ++ printOutAux d out

data Ctx = Top | Bot

showExpr :: Ctx -> Expr -> [Out]
showExpr _ Void = [Str "()"]
showExpr _ (Lit l) = [Str $ show l]
showExpr _ (Var id) = [showIdName id]
showExpr _ (Type ty) = [Str $ "@" ++ show ty]

showExpr _ (Record fields) =
  let showField (id, expr) = [showIdName id,  Str "="] ++ showExpr Top expr ++ [LineBreak]
   in [Str "{", Indent, LineBreak] ++ concatMap showField fields ++ [Dedent, LineBreak, Str "}"]

showExpr Top (App (Var ("#fix", _)) (Lam _ body)) =
  showExpr Top body

showExpr Top (App f arg) =
  showExpr Top f ++ showExpr Bot arg

showExpr Top lam@(Lam _ _) =
  [Str "λ"] ++ aux lam
  where
    aux (Lam (name, T.Type) body) = [Str $ "@" ++ name] ++ aux body
    aux (Lam id body) = [showIdName id] ++ aux body
    aux expr = [Str "-> {", Indent, LineBreak] ++ showExpr Top expr ++ [Dedent, LineBreak, Str "}"]

showExpr Top (Let bind expr) =
  concatMap showBind bind ++ showExpr Top expr

showExpr Top (Match expr cases) =
  [Str "case", Str $ show expr, Str "of {", Indent, LineBreak] ++ concatMap showCase cases ++ [Dedent, LineBreak, Str "}"]

showExpr Bot expr = [Str "("] ++  showExpr Top expr ++ [Str ")"]

showId :: Id -> Out
showId (v, ty) = Str $ v ++ " : " ++ show ty

showIdName :: Id -> Out
showIdName ("#ignore", _) = Str "_"
showIdName (n, _) = Str n

showBind :: Bind -> [Out]
showBind (id, expr) =
  [Str "let", showIdName id] ++ aux expr
    where
      aux (Lam id body) = [showIdName id] ++ aux body
      aux expr = [Str "="] ++ showExpr Top expr ++ [LineBreak]

showCase :: Case -> [Out]
showCase (pat, expr) =
  [Str $ show pat, Str "->", Str $ show expr, LineBreak]

data Pattern
  = PatDefault
  | PatLiteral Literal
  | PatVar Id
  | PatRecord [(Id, Pattern)]
  | PatCtor Id [Pattern]

instance Show Pattern where
  show = showPat Top

showPat :: Ctx -> Pattern -> String
showPat _ PatDefault =
  "_"

showPat _ (PatLiteral l) =
  show l

showPat _ (PatVar (name, _ )) =
  name

showPat _ (PatRecord fields) =
  "{" ++ intercalate ", " (map showField fields) ++ "}"
    where
      showField ((key, _), pat) =
        key ++ ": " ++ showPat Top pat

showPat Top (PatCtor (name, _) pats) =
  name ++ " " ++ unwords (reverse $ map (showPat Bot) pats)

showPat Bot pat@(PatCtor _ _) =
  "(" ++ showPat Top  pat ++ ")"
