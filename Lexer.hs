module Lexer where

import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L

lexer = T.makeTokenParser L.javaStyle

naturalOrFloat = T.naturalOrFloat lexer
