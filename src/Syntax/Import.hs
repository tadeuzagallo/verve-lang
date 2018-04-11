module Syntax.Import (p_imports) where

import Absyn.Untyped
import Syntax.Lexer

import Text.Parsec (choice, option, optionMaybe, sepBy, sepEndBy)
import Text.Parsec.String (Parser)

p_imports :: Parser [Import]
p_imports =
  p_import `sepEndBy` separator

p_import :: Parser Import
p_import = do
  iGlobal <- option False (reserved "global" >> return True)
  reserved "import"
  iModule <- ucid `sepBy` symbol "."
  iAlias <- optionMaybe (reserved "as" >> ucid)
  iItems <- optionMaybe p_importItems
  return $ Import { iGlobal, iModule, iAlias, iItems }

p_importItems :: Parser [ImportItem]
p_importItems =
  braces . commaSep $ p_importItem

p_importItem :: Parser ImportItem
p_importItem = do
  choice [ ImportValue <$> lcid
         , ImportValue <$> parens operator
         , ImportType <$> ucid <*> option [] (parens (commaSep ucid))
         ]
