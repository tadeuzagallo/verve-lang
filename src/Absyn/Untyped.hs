module Absyn.Untyped
  ( module Absyn.Meta
  , module Absyn.Untyped
  , module Absyn.Base
  ) where

import Absyn.Meta

import Absyn.Base

type Id = (Name, Type)

data Type
  = TName Name
  | TApp Type [Type]
  | TArrow [Type] Type
  | TRecord [(Name, Type)]
  | TVoid
  | TPlaceholder
  deriving (Show)

type Module = BaseModule Name Type
type Stmt = BaseStmt Name Type
type Decl = BaseDecl Name Type
type InterfaceItem = BaseInterfaceItem Name Type
type ImplementationItem = BaseImplementationItem Name Type
type DataCtor = BaseDataCtor Name Type
type Param = BaseParam Type
type Generics = BaseGenerics
type Function = BaseFunction Name Type
type Expr = BaseExpr Name Type
type Case = BaseCase Name Type
type Pattern = BasePattern Name
