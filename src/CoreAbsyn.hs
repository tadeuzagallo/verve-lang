module CoreAbsyn where

import Absyn (Id, Literal, Name)
import Types

data Expr
  = Void
  | Lit Literal
  | Var Id
  | App Expr
        Expr
  | Lam Id
        Expr
  | Let [Bind]
        Expr
  {-| Case Expr-}
         {-[Alt]-}
  {-| Type Type-}
  deriving (Show)

type Bind = (Id, Expr)

type Alt = (AltCon, Expr)

data AltCon
  = LitAlt Literal
  | Default
  | ConAlt Name
           [Id]
  deriving (Show)
