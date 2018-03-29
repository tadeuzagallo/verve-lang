module Syntax.Pattern (p_pattern) where

import Absyn.Untyped
import Syntax.Lexer
import Syntax.Literal

import Text.Parsec ((<|>), choice, option)
import Text.Parsec.String (Parser)

p_pattern :: Parser Pattern
p_pattern = choice [ symbol "_" >> return PatDefault
                   , p_literal >>= return . PatLiteral
                   , p_patRecord
                   , p_patList
                   , p_patCtor >>= return . uncurry PatCtor
                   , lcid >>= return . PatVar
                   ]

p_patRecord :: Parser Pattern
p_patRecord =
  let field = do
        key <- lcid
        symbol ":"
        value <- p_pattern
        return (key, value)
   in PatRecord <$> braces (commaSep field)

p_patList :: Parser Pattern
p_patList = do
  let fields =
        option ([], NoRest) (field <|> rest)

      field = do
        pat <- p_pattern
        (pats, rest) <- option ([], NoRest) (comma *> fields)
        return (pat : pats, rest)

      rest = do
        symbol "..."
        rest <- option DiscardRest (NamedRest <$> lcid)
        return ([], rest)

   in uncurry PatList <$> brackets fields

p_patCtor :: Parser (Name, [Pattern])
p_patCtor = do
  ctorName <- ucid
  vars <- option [] . parens . commaSep $ p_pattern
  return (ctorName, vars)
