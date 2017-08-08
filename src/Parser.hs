{-# LANGUAGE NamedFieldPuns #-}

module Parser
  ( parseFile
  , parseStmt
  ) where

import qualified Absyn as A
       (Expr, Function, Module, Stmt, DataCtor)
import Absyn
       hiding (Expr(), Function(), Module(), Stmt(), DataCtor)
import Error
import Lexer
import Types

import Text.Parsec
       (ParseError, (<|>), choice, eof, many, option, parse, try, optionMaybe)
import Text.Parsec.String (Parser, parseFromFile)

type Module = A.Module String
type Stmt = A.Stmt String
type Expr = A.Expr String
type Function = A.Function String
type DataCtor = A.DataCtor String

instance ErrorT ParseError where
  kind _ = "ParseError"

parseFile :: String -> IO (Either Error Module)
parseFile file = do
  result <- parseFromFile p_module file
  return $ either (Left . Error) Right result

parseStmt :: String -> String -> Either Error Stmt
parseStmt file source = liftError $ parse (p_stmt <* eof) file source

p_module :: Parser Module
p_module = Module <$> (many p_stmt <* eof)

p_stmt :: Parser Stmt
p_stmt = choice [ p_enum
                , p_function >>= return . FnStmt
                , p_expr >>= return . Expr
                ]

p_enum :: Parser Stmt
p_enum = do
  reserved "enum"
  name <- ucid
  ctors <- braces . many $ p_constructor
  return $ Enum name ctors

p_constructor :: Parser DataCtor
p_constructor = do
  name <- ucid
  args <- optionMaybe . parens . commaSep $ (p_type [])
  return $ (name, args)

p_function :: Parser Function
p_function = do
  reserved "fn"
  name <- lcid
  generics <- option [] p_generics
  params <- parens $ commaSep (p_typedName generics)
  retType <- option void (p_retType generics)
  body <- braces . many $ p_stmt
  return $ Function {name, generics, params, retType, body}

p_generics :: Parser [Name]
p_generics = angles $ commaSep ucid

p_typedName :: [Name] -> Parser TypedName
p_typedName tvars = do
  name <- lcid
  symbol ":"
  ty <- p_type tvars
  return (name, ty)

p_retType :: [Name] -> Parser Type
p_retType tvars = do
  symbol "->"
  p_type tvars

p_type :: [Name] -> Parser Type
p_type tvars = choice [p_simpleType tvars, p_typeArrow tvars]

p_simpleType :: [Name] -> Parser Type
p_simpleType tvars = do
  name <- ucid
  if elem name tvars
    then return $ Var name
    else return $ Con name

p_typeArrow :: [Name] -> Parser Type
p_typeArrow tvars = do
  tyArgs <- parens $ commaSep (p_type tvars)
  retType <- p_type tvars
  return $ Fun [] tyArgs retType

p_expr :: Parser Expr
p_expr = p_lhs >>= p_rhs

p_lhs :: Parser Expr
p_lhs = choice [p_literal >>= return . Literal, lcid >>= return . Ident]

p_rhs :: Expr -> Parser Expr
p_rhs lhs = (choice [p_app lhs, p_binop lhs] >>= p_rhs) <|> return lhs

p_app :: Expr -> Parser Expr
p_app callee = do
  types <- option [] $ angles (commaSep $ p_type [])
  args <- parens $ commaSep p_expr
  return $ App {callee, types, args}

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
