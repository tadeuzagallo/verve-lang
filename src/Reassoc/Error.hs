module Reassoc.Error
  ( ReassocError(..)
  ) where

import Absyn.Untyped
import Util.Error
import Util.PrettyPrint

data ReassocError
  = UnknownOperator Name
  | PrecedenceError Name Name

instance Show ReassocError where
  show (PrecedenceError p1 p2) =
    "Precedence parsing error: cannot mix `" ++ pprName p1 ++ "` and `" ++ pprName p2 ++ "` in the same infix expression"
  show (UnknownOperator name) =
    "Unknown operator: " ++ name

instance ErrorT ReassocError where
  kind _ = "ReassocError"


