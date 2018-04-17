module Absyn.Untyped
  ( module Absyn.Meta
  , module Absyn.Untyped
  , module Absyn.Base
  , module Absyn.Type
  ) where

import Absyn.Base
import Absyn.Meta
import Absyn.Type

type Module = BaseModule Name
type Stmt = BaseStmt Name
type Decl = BaseDecl Name
type InterfaceItem = BaseInterfaceItem Name
type ImplementationItem = BaseImplementationItem Name
type DataCtor = BaseDataCtor Name
type Param = BaseParam
type Generics = BaseGenerics
type Function = BaseFunction Name
type Expr = BaseExpr Name
type Case = BaseCase Name
type Pattern = BasePattern Name
