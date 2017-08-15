module Lexer
  -- Literals
  ( naturalOrFloat
  , stringLiteral
  , charLiteral
  -- Keywords
  , lcid
  , ucid
  , reserved
  , reservedOp
  , operator
  , symbol
  -- Delimiters
  , parens
  , braces
  , angles
  -- Utils
  , commaSep
  ) where

import Text.Parsec ((<|>), sepEndBy, many, many1, between, try, skipMany, skipMany1, choice)
import Text.Parsec.Char (lower, upper, alphaNum, oneOf, noneOf, char, anyChar, string, digit, satisfy, endOfLine)
import Text.Parsec.String (Parser)

lexeme p = p <* space

space' = choice [ oneOf " \t" >> return ()
                , multiLineComment
                , oneLineComment
                ]

space = skipMany space'

newline = space >> endOfLine >> space

anySpace = skipMany (space' <|> newline)

comment = oneLineComment <|> multiLineComment

oneLineComment = do
  try . string $ "//"
  skipMany . satisfy $ (/= '\n')

multiLineComment = do
  try . string $ "/*"
  content
  return ()
    where
      content :: Parser String
      content = try (string "*/") <|> (anyChar >> content)


-- Literals
naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = lexeme $ Left <$> (many1 digit >>= return . read)

stringLiteral :: Parser String
stringLiteral = lexeme $ between (char '"') (char '"') (many . noneOf $ "\"")

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') (noneOf "'")

-- Delimiters
parens :: Parser a -> Parser a
parens p = lexeme $ between (char '(' <* anySpace) (anySpace *> char ')') p

braces :: Parser a -> Parser a
braces p = lexeme $ between (anySpace *> char '{' *> anySpace) (anySpace *> char '}') p

angles :: Parser a -> Parser a
angles p = lexeme $ between (char '<' <* anySpace) (anySpace *> char '>') p

comma = char ',' <* anySpace

-- Keywords
lcid :: Parser String
lcid = (:) <$> lower <*> idSuffix <* space

ucid :: Parser String
ucid = (:) <$> upper <*> idSuffix <* space

idSuffix :: Parser String
idSuffix = many (alphaNum <|> oneOf "_'")

reserved :: String -> Parser String
reserved name = lexeme . try . string $ name

reservedOp :: String -> Parser String
reservedOp name = lexeme . try . string $ name

operator :: Parser String
operator = lexeme . try . many1 . oneOf $ ":!#$%&*+./<=>?@\\^|-~"

symbol :: String -> Parser String
symbol c = lexeme . try . string $ c

-- Utils
commaSep :: Parser a -> Parser [a]
commaSep = flip sepEndBy comma
