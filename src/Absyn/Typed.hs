module Absyn.Typed
  ( module Absyn.Meta
  , module Absyn.Typed
  , module Absyn.Base
  ) where

import Absyn.Meta
import Typing.Types (Type)

import Absyn.Base

type Id = (Name, Type)

type Module = BaseModule Id Type
type Stmt = BaseStmt Id Type
type Decl = BaseDecl Id Type
type InterfaceItem = BaseInterfaceItem Id Type
type ImplementationItem = BaseImplementationItem Id Type
type DataCtor = BaseDataCtor Id Type
type Param = BaseParam Type
type Generics = BaseGenerics
type Function = BaseFunction Id Type
type Expr = BaseExpr Id Type
type Case = BaseCase Id Type
type Pattern = BasePattern Id
type PatternRest = BasePatternRest Id
