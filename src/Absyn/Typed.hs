module Absyn.Typed
  ( module Absyn.Meta
  , module Absyn.Typed
  , module Absyn.Base
  ) where

import Absyn.Meta
import Typing.Types (Type, Intf)

import Absyn.Base

type Id = (Name, Type)

type Module = BaseModule Id Type Intf
type Stmt = BaseStmt Id Type Intf
type Decl = BaseDecl Id Type Intf
type DataCtor = BaseDataCtor Id Type
type Param = BaseParam Type
type Generics = BaseGenerics Intf
type Function = BaseFunction Id Type Intf
type Expr = BaseExpr Id Type Intf
type Case = BaseCase Id Type Intf
type Pattern = BasePattern Id
