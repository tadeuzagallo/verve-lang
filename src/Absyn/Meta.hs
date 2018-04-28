{-# LANGUAGE PatternSynonyms, RankNTypes #-}

module Absyn.Meta
  ( AST
  , ASTNode
  , NodeVisitor(..)
  , Visit(..)
  , Visitor(..)
  , pattern (:<)
  , getMetaData
  , getNode
  , visitNode
  , defaultAssoc
  , defaultPrec
  ) where

import Absyn.Base

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

type Transform node m fromName toName fromMeta toMeta =
  AST node fromName fromMeta -> m (AST node toName toMeta)

data Visitor m fromName toName fromMeta toMeta =
  Visitor
    { visitModule :: Transform BaseExpr m fromName toName fromMeta toMeta
    , visitStmt :: Transform BaseStmt m fromName toName fromMeta toMeta
    , visitDecl :: Transform BaseDecl m fromName toName fromMeta toMeta
    , visitImplementationItem :: Transform BaseImplementationItem m fromName toName fromMeta toMeta
    , visitFunction :: Transform BaseFunction m fromName toName fromMeta toMeta
    , visitExpr :: Transform BaseExpr m fromName toName fromMeta toMeta
    , visitCase :: Transform BaseCase m fromName toName fromMeta toMeta
    , visitPattern :: Transform BasePattern m fromName toName fromMeta toMeta
    , visitName :: fromName -> m toName
    , visitMetaData :: fromMeta -> m toMeta
    }

type TransformNode node m fromName toName =
  forall a. ASTNode node fromName a -> m (ASTNode node toName a)

data NodeVisitor m fromName toName =
  NodeVisitor
    { visitModuleNode :: TransformNode BaseExpr m fromName toName
    , visitStmtNode :: TransformNode BaseStmt m fromName toName
    , visitDeclNode :: TransformNode BaseDecl m fromName toName
    , visitImplementationItemNode :: TransformNode BaseImplementationItem m fromName toName
    , visitFunctionNode :: TransformNode BaseFunction m fromName toName
    , visitExprNode :: TransformNode BaseExpr m fromName toName
    , visitCaseNode :: TransformNode BaseCase m fromName toName
    , visitPatternNode :: TransformNode BasePattern m fromName toName
    , visitNameNode :: fromName -> m toName
    }

visitNode :: (Monad m, Visit node) => NodeVisitor m fromName toName -> AST node fromName a -> m (AST node toName a)
visitNode v = visit (promoteNodeVisitor v)

promoteNodeVisitor :: Monad m => NodeVisitor m fromName toName -> Visitor m fromName toName a a
promoteNodeVisitor nodeVisitor =
  Visitor { visitModule = wrap $ visitModuleNode nodeVisitor
          , visitStmt = wrap $ visitStmtNode nodeVisitor
          , visitDecl = wrap $ visitDeclNode nodeVisitor
          , visitImplementationItem = wrap $ visitImplementationItemNode nodeVisitor
          , visitFunction = wrap $ visitFunctionNode nodeVisitor
          , visitExpr = wrap $ visitExprNode nodeVisitor
          , visitCase = wrap $ visitCaseNode nodeVisitor
          , visitPattern = wrap $ visitPatternNode nodeVisitor
          , visitName = visitNameNode nodeVisitor
          , visitMetaData = return
          }
  where
    wrap f = \(m :< n) -> (m :<) <$> f n

class Visit (f :: * -> (* -> *) -> *) where
  visit :: Monad m => Visitor m a b x y -> AST f a x -> m (AST f b y)

instance Visit BaseModule where
  visit v (m :< Module imports stmts) = do
    m' <- visitMetaData v m
    stmts' <- mapM (visitStmt v) stmts
    return $ m' :< Module imports stmts'

instance Visit BaseStmt where
  visit v (m :< Decl d) = do
    m' <- visitMetaData v m
    d' <- visitDecl v d
    return $ m' :< Decl d'

  visit v (m :< Expr e) = do
    m' <- visitMetaData v m
    e' <- visitExpr v e
    return $ m' :< Expr e'

instance Visit BaseDecl where
  visit v (m :< FnStmt fn) = do
    m' <- visitMetaData v m
    fn' <- visitFunction v fn
    return $ m' :< FnStmt fn'

  visit v (m :< Enum name gen ctors) = do
    m' <- visitMetaData v m
    return $ m' :< Enum name gen ctors

  visit v (m :< Let decl value) = do
    m' <- visitMetaData v m
    value' <- visitExpr v value
    return $ m' :< Let decl value'

  visit v (m :< cls@(Class { classMethods })) = do
    m' <- visitMetaData v m
    classMethods <- mapM (visitFunction v) classMethods
    return $ m' :< cls { classMethods }

  visit v (m :< op@(Operator { opBody })) = do
    m' <- visitMetaData v m
    opBody <- mapM (visitStmt v) opBody
    return $ m' :< op { opBody }

  visit v (m :< intf@(Interface { intfName })) = do
    -- GHC bug?
    m' <- visitMetaData v m
    return $ m' :< intf { intfName }

  visit v (m :< impl@(Implementation { implIntf, implMethods })) = do
    m' <- visitMetaData v m
    implIntf <- visitName v implIntf
    implMethods <- mapM (visitImplementationItem v) implMethods
    return $ m' :< impl { implIntf , implMethods }

  visit v (m :< typeAlias@(TypeAlias { aliasName })) = do
    -- GHC bug?
    m' <- visitMetaData v m
    return $ m' :< typeAlias { aliasName }

instance Visit BaseImplementationItem where
  visit v (m :< ImplVar (name, value)) = do
    m' <- visitMetaData v m
    value' <- visitExpr v value
    return $ m' :< ImplVar (name, value')

  visit v (m :< fn@(ImplFunction { implBody })) = do
    m' <- visitMetaData v m
    implBody <- mapM (visitStmt v) implBody
    return $ m' :< fn { implBody }

  visit v (m :< op@(ImplOperator { implOpBody })) = do
    m' <- visitMetaData v m
    implOpBody <-  mapM (visitStmt v) implOpBody
    return $ m' :< op { implOpBody }

instance Visit BaseFunction where
  visit v (m :< fn@(Function { body })) = do
    m' <- visitMetaData v m
    body <- mapM (visitStmt v) body
    return $ m' :< fn { body }

instance Visit BaseExpr where
  visit v (m :< Literal l) = do
    m' <- visitMetaData v m
    return $ m' :< Literal l

  visit v (m :< Ident i) = do
    m' <- visitMetaData v m
    return $ m' :< Ident i

  visit v (m :< ParenthesizedExpr e) = do
    m' <- visitMetaData v m
    e' <- visitExpr v e
    return $ m' :< ParenthesizedExpr e'

  visit v (m :< Match expr cases) = do
    m' <- visitMetaData v m
    expr' <- visitExpr v expr
    cases' <- mapM (visitCase v) cases
    return $ m' :< Match expr' cases'

  visit v (m :< If cond conseq alt) = do
    m' <- visitMetaData v m
    cond' <- visitExpr v cond
    conseq' <- mapM (visitStmt v) conseq
    alt' <- mapM (visitStmt v) alt
    return $ m' :< If cond' conseq' alt'

  visit v (m :< call@(Call { callee, args })) = do
    m' <- visitMetaData v m
    callee <- visitExpr v callee
    args <- mapM (visitExpr v) args
    return $ m' :< call { callee, args }

  visit v (m :< binop@(BinOp { op, lhs, rhs })) = do
    m' <- visitMetaData v m
    op <- visitName v op
    lhs <- visitExpr v lhs
    rhs <- visitExpr v rhs
    return $ m' :< binop { op, lhs, rhs }

  visit v (m :< Record fields) = do
    m' <- visitMetaData v m
    fields' <- mapM (mapM (visitExpr v)) fields
    return $ m' :< Record fields'

  visit v (m :< List t items) = do
    m' <- visitMetaData v m
    items' <- mapM (visitExpr v) items
    return $ m' :< List t items'

  visit v (m :< FieldAccess expr field) = do
    m' <- visitMetaData v m
    expr' <- visitExpr v expr
    return $ m' :< FieldAccess expr' field

  visit v (m :< FnExpr fn) = do
    m' <- visitMetaData v m
    fn' <- visitFunction v fn
    return $ m' :< FnExpr fn'

  visit v (m :< Negate constr expr) = do
    m' <- visitMetaData v m
    expr' <- visitExpr v expr
    return $ m' :< Negate constr expr'

  visit v (m :< VoidExpr) = do
    m' <- visitMetaData v m
    return $ m' :< VoidExpr

  visit v (m :< TypeCall expr args) = do
    m' <- visitMetaData v m
    expr' <- visitExpr v expr
    return $ m' :< TypeCall expr' args

instance Visit BaseCase where
  visit v (m :< Case pat body) = do
    m' <- visitMetaData v m
    pat' <- visitPattern v pat
    body' <- mapM (visitStmt v) body
    return $ m' :< Case pat' body'

instance Visit BasePattern where
  visit v (m :< PatDefault) = do
    m' <- visitMetaData v m
    return $ m' :< PatDefault

  visit v (m :< PatLiteral l) = do
    m' <- visitMetaData v m
    return $ m' :< PatLiteral l

  visit v (m :< PatVar x) = do
    m' <- visitMetaData v m
    return $ m' :< PatVar x

  visit v (m :< PatRecord fields) = do
    m' <- visitMetaData v m
    fields' <- mapM (mapM (visitPattern v)) fields
    return $ m' :< PatRecord fields'

  visit v (m :< PatList pats rest) = do
    m' <- visitMetaData v m
    pats' <- mapM (visitPattern v) pats
    return $ m' :< PatList pats' rest

  visit v (m :< PatCtor ctor pats) = do
    m' <- visitMetaData v m
    ctor' <- visitName v ctor
    pats' <- mapM (visitPattern v) pats
    return $ m' :< PatCtor ctor' pats'
