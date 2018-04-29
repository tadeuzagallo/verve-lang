module Syntax.Shared where

import Absyn.Untyped
import Syntax.Lexer
import Syntax.Type
import {-# SOURCE #-} Syntax.Stmt

import Text.Parsec (choice, option)
import Text.Parsec.String (Parser)

-- TODO: add source location
liftParser :: (Visit node) => Parser (ASTNode node name ()) -> Parser (AST node name ())
liftParser f = do
  node <- f
  return $ () :< node

-- Parsers shared across Decl and Expr

p_function :: Parser Function
p_function = liftParser $ do
  reserved "fn"
  name <- lcid
  generics <- option [] p_generics
  params <- parens $ commaSep p_typedName
  retType <- option TVoid p_retType
  body <- p_codeBlock
  return $ Function {name, generics, params, retType, body}

p_codeBlock :: Parser CodeBlock
p_codeBlock = liftParser $ CodeBlock <$> block p_stmt

p_generics :: Parser [(String, [String])]
p_generics = angles $ commaSep p_genericParam

p_genericParam :: Parser (String, [String])
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

