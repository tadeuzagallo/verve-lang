module Lexer
  -- Literals
  ( natural
  , naturalOrFloat
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
  , brackets
  , angles
  -- Utils
  , commaSep
  , commaSep1
  , newline
  , comma
  , anySpace
  ) where

import Text.Parsec ((<|>), sepEndBy1, sepEndBy, many, many1, between, try, skipMany, choice, unexpected)
import Text.Parsec.Char (lower, upper, alphaNum, oneOf, noneOf, char, anyChar, string, digit, satisfy, endOfLine)
import Text.Parsec.String (Parser)

lexeme :: Parser a -> Parser a
lexeme p = p <* space

space' :: Parser ()
space' = choice [ ignore (oneOf " \t")
                , comment
                ]

space :: Parser ()
space = skipMany space'

newline :: Parser ()
newline = space >> endOfLine >> space

anySpace :: Parser ()
anySpace = skipMany (space' <|> newline)

comment :: Parser ()
comment = oneLineComment <|> multiLineComment

oneLineComment :: Parser ()
oneLineComment = do
  ignore . try . string $ "//"
  skipMany . satisfy $ (/= '\n')

multiLineComment :: Parser ()
multiLineComment = do
  ignore . try . string $ "/*"
  ignore content
  return ()
    where
      content :: Parser String
      content = try (string "*/") <|> (anyChar >> content)

-- Literals
naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Left <$> natural

natural :: Parser Integer
natural = lexeme $ many1 digit >>= return . read

stringLiteral :: Parser String
stringLiteral = lexeme $ between (char '"') (char '"') (many . noneOf $ "\"")

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') (noneOf "'")

-- Delimiters
parens :: Parser a -> Parser a
parens p = lexeme $ between (char '(' <* anySpace) (anySpace *> char ')') p

braces :: Parser a -> Parser a
braces p = lexeme $ between (char '{' <* anySpace) (anySpace *> char '}') p

brackets :: Parser a -> Parser a
brackets p = lexeme $ between (char '[' <* anySpace) (anySpace *> char ']') p

angles :: Parser a -> Parser a
angles p = lexeme $ between (char '<' <* anySpace) (anySpace *> char '>') p

comma :: Parser ()
comma = ignore (char ',' <* anySpace)

-- Keywords
reservedKeywords :: [String]
reservedKeywords = [ "fn"
                   , "match"
                   , "case"
                   , "if"
                   , "else"
                   , "operator"
                   , "let"
                   , "class"
                   , "enum"
                   , "type"
                   ]

checkKeyword :: String -> Parser String
checkKeyword word =
  if word `elem` reservedKeywords
     then unexpected ("reserved keyword " ++ show word)
     else return word

lcid :: Parser String
lcid = lexeme . try $ (:) <$> lower <*> idSuffix >>= checkKeyword

ucid :: Parser String
ucid = lexeme . try $ (:) <$> upper <*> idSuffix

idSuffix :: Parser String
idSuffix = many (alphaNum <|> oneOf "_'")

reserved :: String -> Parser ()
reserved name = ignore . lexeme . try . string $ name

reservedOp :: String -> Parser ()
reservedOp name = ignore . lexeme . try . string $ name

operator :: Parser String
operator = lexeme . try . many1 . oneOf $ ":!#$%&*+./<=>?@\\^|-~"

symbol :: String -> Parser ()
symbol c = ignore . lexeme . try . string $ c

-- Utils
commaSep :: Parser a -> Parser [a]
commaSep = flip sepEndBy comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 = flip sepEndBy1 comma

ignore :: Parser a -> Parser ()
ignore p = p >> return ()
