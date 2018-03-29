module Syntax.Type
  ( p_type
  , p_retType
  , p_typedName
  , p_typeAnnotation
  ) where

import Absyn.Untyped
import Syntax.Lexer

import Text.Parsec ((<|>), choice)
import Text.Parsec.String (Parser)

p_type :: Parser Type
p_type = choice [p_simpleType, p_typeArrow, p_typeRecord]

p_simpleType :: Parser Type
p_simpleType = ucid >>= p_typeApp . TName

p_typeApp :: Type -> Parser Type
p_typeApp ty = do
  angles (commaSep p_type) >>= return . TApp ty
  <|> return ty

p_typeArrow :: Parser Type
p_typeArrow = do
  tyArgs <- parens $ commaSep p_type
  retType <- p_retType
  return $ TArrow tyArgs retType

p_typeRecord :: Parser Type
p_typeRecord = do
  fields <- braces $ commaSep p_typedName
  return $ TRecord fields

p_typedName :: Parser Param
p_typedName = do
  name <- lcid
  ty <- p_typeAnnotation
  return (name, ty)

p_typeAnnotation :: Parser Type
p_typeAnnotation = do
  symbol ":"
  p_type

p_retType :: Parser Type
p_retType = do
  reservedOp "->"
  p_type
