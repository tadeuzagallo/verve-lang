module Core.Absyn where

import Absyn.Typed (Id, Literal)
import Typing.Types (Type)
import Util.PrettyPrint

import Data.List (intercalate)

newtype Var = Var String
  deriving (Eq, Show)

newtype ContVar = ContVar String
  deriving (Eq, Show)

data Term
  = LetVal Var Value Term
  | LetCont [ContDef] Term
  | LetFun [FunDef] Term
  | AppCont ContVar [Var]
  | App Var ContVar [Var]
  | Match Var [(Pattern, ContVar)]
  deriving (Show)

data FunDef = FunDef Var ContVar [Var] Term
  deriving (Show)

data ContDef = ContDef ContVar [Var] Term
  deriving (Show)

data Lambda = Lambda ContVar [Var] Term
  deriving (Show)

data Value
  = Unit
  | Lit Literal
  | In String [Var]
  | Lam Lambda
  | Record [(Id, Var)]
  | Type Type
  deriving (Show)

data Pattern
  = PatDefault
  | PatLiteral Literal
  | PatVar Var
  | PatRecord [Var]
  | PatCtor Var
  deriving (Show)

-- Pretty Printing

instance PrettyPrint Var where
  ppr (Var v) = v

instance PrettyPrint ContVar where
  ppr (ContVar v) = v

instance PrettyPrint Term where
  ppr (LetVal x v t) =
    "letval " ++ ppr x ++ " = " ++ ppr v ++ " in\n" ++
      ppr t

  ppr (LetCont contDefs t) =
    "letcont\n" ++
      concatMap ppr contDefs ++ "\n" ++
        "in " ++ ppr t

  ppr (LetFun funDefs t) =
    "letfun\n" ++
      concatMap ppr funDefs ++ "\n" ++
        "in " ++ ppr t

  ppr (AppCont k args) =
    ppr k ++ " " ++ unwords (map ppr args)

  ppr (App x k args) =
    ppr x ++ " " ++ ppr k ++ " " ++ unwords (map ppr args)

  ppr (Match x cases) =
    "match " ++ ppr x ++ " with\n" ++
      unlines (map pprCase cases)
      where pprCase (pat, k) =
              ppr pat ++ " -> " ++ ppr k

instance PrettyPrint FunDef where
  ppr (FunDef f k params body) =
    unwords (ppr f : ppr k : map ppr (params)) ++ " = " ++ ppr body

instance PrettyPrint ContDef where
  ppr (ContDef k params body) =
    unwords (ppr k : map ppr params) ++ " = " ++ ppr body

instance PrettyPrint Lambda where
  ppr (Lambda k params body) =
    "λ " ++ ppr k ++ " " ++ unwords (map ppr params) ++ " . " ++ ppr body

instance PrettyPrint Value where
  ppr Unit = "()"

  ppr (Lit lit) = show lit

  ppr (In i vars) = "in " ++ i ++ " " ++ unwords (map ppr vars)

  ppr (Lam l) = ppr l

  ppr (Record fields) =
    "{ " ++ intercalate ", " (map pprField fields) ++ " }"
      where pprField ((n, _), var) =
              n ++ ": " ++ ppr var

  ppr (Type t) = ppr t

instance PrettyPrint Pattern where
  ppr PatDefault = "_"

  ppr (PatLiteral l) = show l

  ppr (PatVar v) = ppr v
  
  ppr (PatRecord r) =
    "{ " ++ intercalate ", " (map ppr r) ++ " }"

  ppr (PatCtor v) = ppr v
