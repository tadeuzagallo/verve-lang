{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  ) where

import Absyn
import Lexer

import Text.Parsec (ParseError, (<|>), choice, eof, try)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either ParseError Expr)
parseFile = parseFromFile p_program

p_program :: Parser Expr
p_program = p_expr <* eof

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
    [ integer >>= return . Integer
    , float >>= return . Float
    , charLiteral >>= return . Char
    , stringLiteral >>= return . String
    ]
