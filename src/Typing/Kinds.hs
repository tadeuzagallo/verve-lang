module Typing.Kinds 
  ( Kind(Star)
  , kindOf
  ) where

import Typing.Types

data Kind
  = Star
  | Arr Kind Kind
  deriving (Eq)

instance Show Kind where
  show = showKind False

showKind :: Bool -> Kind -> String
showKind _ Star = "*"

showKind False (Arr k1 k2) =
  showKind True k1 ++ " => " ++ show k2

showKind True k =
  "(" ++ show k ++ ")"

kindOf :: Type -> Kind
kindOf (TyAbs params ty) =
  foldr (flip Arr) (kindOf ty) (map (const Star) params)

kindOf _ =
  Star
