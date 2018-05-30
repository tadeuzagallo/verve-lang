{-# LANGUAGE PatternSynonyms, RankNTypes #-}

module Absyn.Meta
  ( module Absyn.Loc
  , AST
  , ASTNode
  , pattern (:<)
  , getMetaData
  , getNode
  , defaultAssoc
  , defaultPrec
  ) where

import Absyn.Base
import Absyn.Loc

defaultPrec :: Precedence
defaultPrec = PrecValue 50

defaultAssoc :: Associativity
defaultAssoc = AssocLeft

data Annotated (a :: *) (f :: *) =
  a :< f

getMetaData :: AST node name meta -> meta
getMetaData (m :< _) = m

getNode :: AST node name meta -> ASTNode node name meta
getNode (_ :< node) = node

type AST (node :: * -> (* -> *) -> *) (name :: *) (meta :: *) =
  Annotated meta (node name (Annotated meta))

type ASTNode (node :: * -> (* -> *) -> *) (name :: *) (meta :: *) =
  node name (Annotated meta)
