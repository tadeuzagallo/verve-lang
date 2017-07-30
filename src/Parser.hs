{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  ) where

import Absyn
import Lexer
import Types

import Text.Parsec
       (ParseError, (<|>), choice, eof, many, option, try)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either ParseError Module)
parseFile = parseFromFile p_module

p_module :: Parser Module
p_module = Module <$> (many p_stmt <* eof)

p_stmt :: Parser Stmt
p_stmt = choice [p_function >>= return . FnStmt, p_expr >>= return . Expr]

p_function :: Parser Function
p_function = do
  reserved "fn"
  name <- identifier
  params <- parens $ commaSep p_typedName
  retType <- option void p_retType
  body <- braces $ many p_stmt
  return $ Function {name, params, retType, body}

p_typedName :: Parser TypedName
p_typedName = do
  name <- identifier
  symbol ":"
  ty <- p_type
  return $ TypedName name ty

p_retType :: Parser Type
p_retType = do
  symbol "->"
  p_type

p_type :: Parser Type
p_type = choice [identifier >>= return . Con, p_typeArrow]

p_typeArrow :: Parser Type
p_typeArrow = do
  tyArgs <- parens $ commaSep p_type
  retType <- p_type
  return $ Arr tyArgs retType

p_expr :: Parser Expr
p_expr = do
  lhs <- p_lhs
  p_rhs lhs

p_lhs :: Parser Expr
p_lhs = choice [p_literal >>= return . Literal, identifier >>= return . Ident]

p_rhs :: Expr -> Parser Expr
p_rhs lhs = (choice [p_app lhs, p_binop lhs] >>= p_rhs) <|> return lhs

p_app :: Expr -> Parser Expr
p_app callee = do
  args <- parens $ commaSep p_expr
  return $ App {callee, args}

p_binop :: Expr -> Parser Expr
p_binop lhs = do
  op <- try operator
  rhs <- p_expr
  return $ BinOp {lhs, op, rhs}

p_literal :: Parser Literal
p_literal =
  choice
    [p_number, charLiteral >>= return . Char, stringLiteral >>= return . String]

p_number :: Parser Literal
p_number = do
  number <- naturalOrFloat
  case number of
    Left int -> return $ Integer int
    Right float -> return $ Float float
