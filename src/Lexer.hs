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

import Data.Char (digitToInt)
import Text.Parsec ((<|>), (<?>), sepEndBy1, sepEndBy, many, many1, between, try, skipMany, choice, unexpected, parserFail, option, parserZero)
import Text.Parsec.Char (lower, upper, alphaNum, oneOf, noneOf, char, anyChar, string, digit, satisfy, endOfLine, hexDigit, octDigit)
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
  content
    where
      content :: Parser ()
      content = choice [ multiLineComment >> content
                       , ignore $ try (string "*/")
                       , ignore $ (anyChar >> content)
                       , parserFail "unterminated multiline comment"
                       ]

-- Literals
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

-- From Parsec
naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat  = lexeme (natFloat) <?> "number"

natural :: Parser Integer
natural         = lexeme nat        <?> "natural"


-- floats
natFloat :: Parser (Either Integer Double)
natFloat        = do{ _ <- char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat :: Parser (Either Integer Double)
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat :: Parser (Either Integer Double)
decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat :: Integer -> Parser (Either Integer Double)
fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent :: Integer -> Parser Double
fractExponent n = do{ fract <- fraction
                    ; expo  <- option "" exponent'
                    ; readDouble (show n ++ fract ++ expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; readDouble (show n ++ expo)
                    }
                  where
                    readDouble s =
                      case reads s of
                        [(x, "")] -> return x
                        _         -> parserZero

fraction :: Parser String
fraction        = do{ _ <- char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return ('.' : digits)
                    }
                  <?> "fraction"

exponent' :: Parser String
exponent'       = do{ _ <- oneOf "eE"
                    ; sign' <- fmap (:[]) (oneOf "+-") <|> return ""
                    ; e <- decimal <?> "exponent"
                    ; return ('e' : sign' ++ show e)
                    }
                  <?> "exponent"


-- integers and naturals
nat :: Parser Integer
nat             = zeroNumber <|> decimal

zeroNumber :: Parser Integer
zeroNumber      = do{ _ <- char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal, hexadecimal, octal :: Parser Integer
decimal         = number 10 digit
hexadecimal     = do{ _ <- oneOf "xX"; number 16 hexDigit }
octal           = do{ _ <- oneOf "oO"; number 8 octDigit  }

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }
