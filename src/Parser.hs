module Parser
    ( parseFile
    ) where

import Absyn
import Lexer

import Text.Parsec (choice, eof, ParseError)
import Text.Parsec.String (Parser, parseFromFile)

parseFile :: String -> IO (Either ParseError Literal)
parseFile = parseFromFile program

program :: Parser Literal
program = literal <* eof

literal :: Parser Literal
literal = choice [ integer >>= return . Integer
                 , float >>= return . Float
                 , charLiteral >>= return . Char
                 , stringLiteral >>= return . String
                 ]
