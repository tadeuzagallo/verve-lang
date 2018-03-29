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
                          , p_function >>= return . FnExpr
                          , p_lhs allowCtor >>= p_rhs allowCtor
                          ]

p_lhs :: Bool -> Parser Expr
p_lhs allowCtor = choice [ p_record
                         , p_list
                         , p_literal >>= return . Literal
                         , try p_lcid
                         , p_ucid allowCtor
                         , parens p_parenthesizedExpr
                         , p_negate allowCtor
                         ]

p_lcid :: Parser Expr
p_lcid = do
  prefix <- ucid `endBy` symbol "."
  var <- lcid
  return $ Ident (prefix ++ [var]) TPlaceholder

p_ucid :: Bool -> Parser Expr
p_ucid allowCtor = do
  parts <- ucid `sepBy1` symbol "."
  let ident = Ident parts TPlaceholder
  if allowCtor
     then p_ctor ident <|> return ident
     else return ident

p_parenthesizedExpr :: Parser Expr
p_parenthesizedExpr = do
  choice [ ParenthesizedExpr <$> p_expr True
         , operator >>= return . flip Ident TPlaceholder . (:[])
         ]

p_negate :: Bool -> Parser Expr
p_negate allowCtor = try $ do { symbol "-"
                              ; Negate [] <$> p_expr allowCtor
                              }

p_ctor :: Expr -> Parser Expr
p_ctor ctor = do
  r <- p_record
  return (Call ctor [] [] [r])

p_rhs :: Bool -> Expr -> Parser Expr
p_rhs allowCtor lhs = (choice [try $ p_call lhs, p_fieldAccess lhs, p_binop allowCtor lhs] >>= p_rhs allowCtor) <|> return lhs

p_record :: Parser Expr
p_record =
  Record <$> braces (commaSep field)
    where
      field = (,) <$> lcid <*> (symbol ":" *> p_expr True)

p_list :: Parser Expr
p_list =
  List TPlaceholder <$> (brackets . commaSep $ p_expr True)

p_call :: Expr -> Parser Expr
p_call callee = do
  (typeArgs, args) <- p_callArgs
  return $ Call {callee, constraintArgs = [], typeArgs, args}

p_binop :: Bool -> Expr -> Parser Expr
p_binop allowCtor lhs = do
  op <- operator
  rhs <- p_expr allowCtor
  return $ BinOp {opConstraintArgs = [], opTypeArgs = [], lhs, op, rhs}

p_fieldAccess :: Expr -> Parser Expr
p_fieldAccess lhs = do
  symbol "."
  fieldName <- lcid
  p_methodCall lhs fieldName <|> return (FieldAccess lhs TPlaceholder fieldName)

p_methodCall :: Expr -> Name -> Parser Expr
p_methodCall lhs name = do
  (typeArgs, args) <- p_callArgs
  return $ Call { callee = Ident [name] TPlaceholder, constraintArgs = [], typeArgs, args = lhs : args }

p_callArgs :: Parser ([Type], [Expr])
p_callArgs = do
  typeArgs <- option [] $ angles (commaSep $ p_type)
  args <- parens $ commaSep (p_expr True)
  return (typeArgs, args)

p_match :: Parser Expr
p_match = do
  reserved "match"
  expr <- p_expr False
  cases <- block p_case
  return $ Match { expr, cases }

p_case :: Parser Case
p_case = do
  reserved "case"
  pattern <- p_pattern
  symbol ":"
  caseBody <- p_caseBody True
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
p_if = do
  reserved "if"
  ifCond <- p_expr False
  ifBody <- p_codeBlock
  ifElseBody <- option [] p_else
  return $ If { ifCond, ifBody, ifElseBody }

p_else :: Parser [Stmt]
p_else = do
  reserved "else"
  p_codeBlock <|> ((:[]) <$> (Expr <$> p_if))
