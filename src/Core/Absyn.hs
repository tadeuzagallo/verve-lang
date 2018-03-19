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
  | Case Var [Clause]
  | Error
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

data Clause = Clause String ContVar
  deriving (Show)

-- Pretty Printing
instance PrettyPrint Var where
  ppr (Var v) = v

instance PrettyPrint ContVar where
  ppr (ContVar v) = v

instance PrettyPrint Term where
  ppr Error = "error"

  ppr (LetVal x v t) =
    "letval " ++ ppr x ++ " = " ++ ppr v ++ " in\n" ++
      ppr t

  ppr (LetCont contDefs t) =
    "letcont\n" ++
      unlines (map ppr contDefs) ++ "\n" ++
        "in " ++ ppr t

  ppr (LetFun funDefs t) =
    "letfun\n" ++
      concatMap ppr funDefs ++ "\n" ++
        "in " ++ ppr t

  ppr (AppCont k args) =
    ppr k ++ " " ++ unwords (map ppr args)

  ppr (App x k args) =
    ppr x ++ " " ++ ppr k ++ " " ++ unwords (map ppr args)

  ppr (Case x cases) =
    "case " ++ ppr x ++ " of\n" ++
      unlines (map pprCase cases)
        where
          pprCase (Clause c k) =
            c ++ " -> " ++ ppr k

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

-- Substitution
class Subst t where
  -- t[v / x]
  subst :: t -> Var -> Var -> t

instance Subst Term where
  subst Error _ _ = Error

  subst (LetVal x' v' t) v x =
    LetVal x' v' (if x' == x then t else subst t v x)

  subst (LetCont d t) v x =
    LetCont (subst d v x) (subst t v x)

  subst (LetFun d t) v x =
    if x `elem` map fnName d
       then LetFun d t
       else LetFun (subst d v x) (subst t v x)
     where fnName (FunDef n _ _ _) = n

  subst (AppCont k xs) v x =
    AppCont k (subst xs v x)

  subst (App f k xs) v x =
    App (subst f v x) k (subst xs v x)

  subst (Case y cs) v x =
    Case (subst y v x) cs

instance Subst ContDef where
  subst (ContDef k xs t) v x =
    ContDef k xs (if x `elem` xs then t else subst t v x)

instance Subst FunDef where
  subst (FunDef f k xs t) v x =
    FunDef f k xs (if x `elem` xs then t else subst t v x)

instance Subst Var where
  subst y v x
    | x == y = v
    | otherwise = y

instance (Subst t) => Subst [t] where
  subst ts v x = map (\t -> subst t v x) ts
