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

type Module = BaseModule Name Type Name
type Stmt = BaseStmt Name Type Name
type Decl = BaseDecl Name Type Name
type DataCtor = BaseDataCtor Name Type
type Param = BaseParam Type
type Generics = BaseGenerics Name
type Function = BaseFunction Name Type Name
type Expr = BaseExpr Name Type Name
type Case = BaseCase Name Type Name
type Pattern = BasePattern Name
