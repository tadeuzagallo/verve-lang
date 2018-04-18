module Core.Absyn where

import Prelude hiding (concat)

import Absyn.Typed (Literal)
import Typing.Types (Type)
import Util.PrettyPrint

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
  | Record [(String, Var)]
  | Type Type
  deriving (Show)

data Clause = Clause String ContVar
  deriving (Show)

-- Pretty Printing
instance PrettyPrint Var where
  pprint (Var v) = str v

instance PrettyPrint ContVar where
  pprint (ContVar v) = str v

instance PrettyPrint Term where
  pprint Error = str "error"

  pprint (LetVal x v t) =
    concat [ str "letval ",  pprint x, str " = ", pprint v, str " in", newline, pprint t ]

  pprint (LetCont contDefs t) =
    concat [ str "letcont"
           , indent (newline `append` interleave newline (map pprint contDefs))
           , newline, str "in", newline, pprint t
           ]

  pprint (LetFun funDefs t) =
    concat [ str "letfun"
           , indent (newline `append` interleave newline (map pprint funDefs))
           , newline, str "in", newline, pprint t
           ]

  pprint (AppCont k args) =
    concat [pprint k, str " ", interleave (str " ") (map pprint args)]

  pprint (App x k args) =
    concat [pprint x, str " ", pprint k, str " ", interleave (str " ") (map pprint args)]

  pprint (Case x cases) =
    concat [str "case ", pprint x, str " of"
           , indent (newline `append` interleave newline (map pprint cases))
           ]

instance PrettyPrint Clause where
  pprint (Clause c k) =
    concat [str c, str " -> ", indent (newline `append` pprint k)]

instance PrettyPrint FunDef where
  pprint (FunDef f k params body) =
    concat [ interleave (str " ") (pprint f : pprint k : map pprint params)
           , str " = " , indent (newline `append` pprint body)
           ]

instance PrettyPrint ContDef where
  pprint (ContDef k params body) =
    concat [interleave (str " ") (pprint k : map pprint params)
           , str " = ",  indent (newline `append` pprint body)
           ]

instance PrettyPrint Lambda where
  pprint (Lambda k params body) =
    concat [str "λ "
           , interleave (str " ") (pprint k : map pprint params)
           , str " . ", indent (newline `append` pprint body)
           ]

instance PrettyPrint Value where
  pprint Unit = str "()"

  pprint (Lit lit) = str (show lit)

  pprint (In i vars) = concat [ str i, str "(",  interleave (str ", ") (map pprint vars), str ")" ]

  pprint (Lam l) = pprint l

  pprint (Record fields) =
    concat [ str "{"
           , indent (newline `append` interleave (str "," `append` newline) (map pprintField fields))
           , newline
           , str "}"
           ]
      where
        pprintField (n, var) =
          concat [str n, str ": ", pprint var]

  pprint (Type t) = pprint t

-- Substitution
class Subst t where
  -- t[v / x]
  subst :: t -> Var -> Var -> t

instance Subst Term where
  subst Error _ _ = Error

  subst (LetVal x' v' t) v x =
    LetVal x' (subst v' v x) (if x' == x then t else subst t v x)

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

instance Subst Value where
  subst Unit _ _ = Unit
  subst (Lit l) _ _ = Lit l
  subst (In c vs) v x = In c (subst vs v x)
  subst (Lam t) v x = Lam (subst t v x)
  subst (Record t) v x = Record (subst t v x)
  subst (Type t) _ _ = Type t

instance Subst Lambda where
  subst (Lambda k xs t) v x =
    Lambda k xs (if x `elem` xs then t else subst t v x)

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

instance Subst t => Subst (x, t) where
  subst (r, t) v x = (r, subst t v x)

instance (Subst t) => Subst [t] where
  subst ts v x = map (\t -> subst t v x) ts
