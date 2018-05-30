module Syntax.Expr (p_expr) where

import Absyn.Untyped
import Syntax.Lexer
import Syntax.Literal
import Syntax.Pattern
import Syntax.Shared
import {-# SOURCE #-} Syntax.Stmt
import Syntax.Type

import Text.Parsec ((<|>), choice, endBy, lookAhead, option, optional, sepBy1, try)
import Text.Parsec.String (Parser)

p_expr :: Bool -> Parser Expr
p_expr allowCtor = choice [ p_match
                          , p_if
                          , liftParser $ FnExpr <$> p_function
                          , p_lhs allowCtor >>= p_rhs allowCtor
                          ]

p_lhs :: Bool -> Parser Expr
p_lhs allowCtor = choice [ p_record
                         , p_list
                         , liftParser $ Literal <$> p_literal
                         , try p_lcid
                         , p_ucid allowCtor
                         , parens p_parenthesizedExpr
                         , p_negate allowCtor
                         ]

p_lcid :: Parser Expr
p_lcid = liftParser $ do
  prefix <- ucid `endBy` symbol "."
  var <- lcid
  return $ Ident (prefix ++ [var])

p_ucid :: Bool -> Parser Expr
p_ucid allowCtor = do
  parts <- ucid `sepBy1` symbol "."
  ident <- (liftParser . return) (Ident parts)
  if allowCtor
     then p_ctor ident <|> return ident
     else return ident

p_parenthesizedExpr :: Parser Expr
p_parenthesizedExpr = liftParser $ do
  choice [ ParenthesizedExpr <$> p_expr True
         , operator >>= return . Ident . (:[])
         ]

p_negate :: Bool -> Parser Expr
p_negate allowCtor = liftParser . try $ do
  symbol "-"
  Negate [] <$> p_expr allowCtor

p_ctor :: Expr -> Parser Expr
p_ctor ctor = liftParser $ do
  r <- p_record
  return (Call ctor [] [] [r])

p_rhs :: Bool -> Expr -> Parser Expr
p_rhs allowCtor lhs = (choice [try $ p_call lhs, p_fieldAccess lhs, p_binop allowCtor lhs] >>= p_rhs allowCtor) <|> return lhs

p_record :: Parser Expr
p_record = liftParser $
  Record <$> braces (commaSep field)
    where
      field = (,) <$> lcid <*> (symbol ":" *> p_expr True)

p_list :: Parser Expr
p_list = liftParser $
  List Nothing <$> (brackets . commaSep $ p_expr True)

p_call :: Expr -> Parser Expr
p_call callee = liftParser $ do
  (typeArgs, args) <- p_callArgs
  return $ Call {callee, constraintArgs = [], typeArgs, args}

p_binop :: Bool -> Expr -> Parser Expr
p_binop allowCtor lhs = liftParser $ do
  op <- operator
  rhs <- p_expr allowCtor
  return $ BinOp {opConstraintArgs = [], opTypeArgs = [], lhs, op, rhs}

p_fieldAccess :: Expr -> Parser Expr
p_fieldAccess lhs = do
  symbol "."
  fieldName <- lcid
  p_methodCall lhs fieldName <|> (liftParser . return) (FieldAccess lhs fieldName)

p_methodCall :: Expr -> String -> Parser Expr
p_methodCall lhs name = liftParser $ do
  (typeArgs, args) <- p_callArgs
  callee <- (liftParser . return) (Ident [name])
  return $ Call { callee
                , constraintArgs = []
                , typeArgs
                , args = lhs : args
                }

p_callArgs :: Parser ([Type], [Expr])
p_callArgs = do
  typeArgs <- option [] $ angles (commaSep $ p_type)
  args <- parens $ commaSep (p_expr True)
  return (typeArgs, args)

p_match :: Parser Expr
p_match = liftParser $ do
  reserved "match"
  expr <- p_expr False
  cases <- block p_case
  return $ Match { expr, cases }

p_case :: Parser Case
p_case = liftParser $ do
  reserved "case"
  pattern <- p_pattern
  symbol ":"
  caseBody <- liftParser $ CodeBlock <$> p_caseBody True
  return $ Case { pattern, caseBody }

p_caseBody :: Bool -> Parser [Stmt]
p_caseBody first = do
  (lookAheadCase >> return [])
  <|>
    (option [] ((:) <$> (try $ sep >> p_stmt) <*> p_caseBody False))
    where
      lookAheadCase = lookAhead . try $ optional separator >> reserved "case"
      sep
        | first = optional separator
        | otherwise = separator


p_if :: Parser Expr
p_if = liftParser $ do
  reserved "if"
  ifCond <- p_expr False
  ifBody <- p_codeBlock
  emptyBlock <- liftParser $ return $ CodeBlock []
  ifElseBody <- option emptyBlock p_else
  return $ If { ifCond, ifBody, ifElseBody }

p_else :: Parser CodeBlock
p_else = do
  reserved "else"
  p_codeBlock <|> (liftParser $ CodeBlock . (:[]) <$> (liftParser $ Expr <$> p_if))
