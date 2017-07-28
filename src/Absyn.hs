module Absyn (Literal(..)
             ) where

data Literal
  = Integer Integer 
  | Float Double
  | Char Char
  | String String
  deriving (Show)
