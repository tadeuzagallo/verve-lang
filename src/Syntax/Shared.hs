module Syntax.Shared where

import Absyn.Untyped
import Syntax.Lexer
import Syntax.Type
import {-# SOURCE #-} Syntax.Stmt

import Text.Parsec (choice, option)
import Text.Parsec.String (Parser)

-- Parsers shared across Decl and Expr

p_function :: Parser Function
p_function = do
  reserved "fn"
  name <- lcid
  generics <- option [] p_generics
  params <- parens $ commaSep p_typedName
  retType <- option TVoid p_retType
  body <- p_codeBlock
  return $ Function {name, generics, params, retType, body}

p_codeBlock :: Parser [Stmt]
p_codeBlock = block p_stmt

p_generics :: Parser [(Name, [String])]
p_generics = angles $ commaSep p_genericParam

p_genericParam :: Parser (Name, [String])
p_genericParam = do
  var <- ucid
  bounds <- option [] p_genericBounds
  return (var, bounds)

p_genericBounds :: Parser [String]
p_genericBounds = do
  symbol ":"
  choice [ parens $ commaSep1 ucid
         , ucid >>= return . (:[])
         ]

