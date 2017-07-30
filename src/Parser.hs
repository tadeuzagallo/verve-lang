{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  ) where

import Absyn
import Lexer
import Types

import Data.List (elemIndex)
import Text.Parsec
       (ParseError, (<|>), choice, eof, many, option, try)
import Text.Parsec.String (Parser, parseFromFile)

type Ctx = [String]

emptyCtx :: Ctx
emptyCtx = []

addParams :: Ctx -> [TypedName] -> Ctx
addParams ctx params =
  let paramNames = map (\(TypedName n _) -> n) params
  in reverse paramNames ++ ctx

type ParserT a = Ctx -> Parser a

parseFile :: String -> IO (Either ParseError Module)
parseFile = parseFromFile p_module

p_module :: Parser Module
p_module = Module <$> (many (p_stmt emptyCtx) <* eof)

p_stmt :: ParserT Stmt
p_stmt ctx =
  choice [p_function ctx >>= return . FnStmt, p_expr ctx >>= return . Expr]

p_function :: ParserT Function
p_function ctx = do
  reserved "fn"
  name <- identifier
  params <- parens $ commaSep p_typedName
  retType <- option void p_retType
  body <- braces . many $ p_stmt (addParams ctx params)
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

p_expr :: ParserT Expr
p_expr ctx = do
  lhs <- p_lhs ctx
  p_rhs ctx lhs

p_lhs :: ParserT Expr
p_lhs ctx =
  choice [p_literal >>= return . Literal, p_name ctx >>= return . Ident]

p_rhs :: Ctx -> Expr -> Parser Expr
p_rhs ctx lhs =
  (choice [p_app ctx lhs, p_binop ctx lhs] >>= p_rhs ctx) <|> return lhs

p_app :: Ctx -> Expr -> Parser Expr
p_app ctx callee = do
  args <- parens $ commaSep (p_expr ctx)
  return $ App {callee, args}

p_binop :: Ctx -> Expr -> Parser Expr
p_binop ctx lhs = do
  op <- try operator
  rhs <- p_expr ctx
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

p_name :: ParserT Name
p_name ctx = do
  ident <- identifier
  return $
    case elemIndex ident ctx of
      Nothing -> Global ident
      Just i -> Local i
