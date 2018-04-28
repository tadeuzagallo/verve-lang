module Syntax.Pattern (p_pattern) where

import Absyn.Untyped
import Syntax.Lexer
import Syntax.Literal
import Syntax.Shared

import Text.Parsec ((<|>), choice, option)
import Text.Parsec.String (Parser)

p_pattern :: Parser Pattern
p_pattern =
  choice [ p_patDefault
         , p_patLiteral
         , p_patRecord
         , p_patList
         , p_patCtor
         , p_patVar
         ]

p_patDefault :: Parser Pattern
p_patDefault = liftParser $ do
  symbol "_"
  return PatDefault

p_patLiteral :: Parser Pattern
p_patLiteral = liftParser $ do
  l <- p_literal
  return $ PatLiteral l

p_patRecord :: Parser Pattern
p_patRecord = liftParser $
  let field = do
        key <- lcid
        symbol ":"
        value <- p_pattern
        return (key, value)
   in PatRecord <$> braces (commaSep field)

p_patList :: Parser Pattern
p_patList = liftParser $
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

p_patCtor :: Parser Pattern
p_patCtor = liftParser $ do
  ctorName <- ucid
  vars <- option [] . parens . commaSep $ p_pattern
  return $ PatCtor ctorName vars

p_patVar :: Parser Pattern
p_patVar = liftParser $ do
  PatVar <$> lcid
