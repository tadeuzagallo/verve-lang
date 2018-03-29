module Core.Match (match) where

import qualified Core.Absyn as CA
import Core.State
import Absyn.Typed

type Equation = ([Pattern], CA.Term)

match :: [CA.Var] -> [Equation] -> CA.Term -> DsM CA.Term
