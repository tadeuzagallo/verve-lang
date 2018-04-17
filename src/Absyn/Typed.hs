module Absyn.Typed
  ( module Absyn.Meta
  , module Absyn.Typed
  , module Absyn.Base
  ) where

import Absyn.Meta
import Typing.Types (Type)

import Absyn.Base

type Id = (Name, Type)

type Module = BaseModule Id
type Stmt = BaseStmt Id
type Decl = BaseDecl Id
type InterfaceItem = BaseInterfaceItem Id
type ImplementationItem = BaseImplementationItem Id
type DataCtor = BaseDataCtor Id
type Param = BaseParam
type Generics = BaseGenerics
type Function = BaseFunction Id
type Expr = BaseExpr Id
type Case = BaseCase Id
type Pattern = BasePattern Id
type PatternRest = BasePatternRest Id
