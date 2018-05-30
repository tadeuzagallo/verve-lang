module Absyn.Loc
  ( SourcePos
  , SourceSpan(..)
  ) where

import Text.Parsec (SourcePos)

data SourceSpan = SourceSpan { spanStart :: SourcePos
                             , spanEnd :: SourcePos
                             }
