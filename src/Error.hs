{-# LANGUAGE GADTs #-}

module Error
  ( Error(..)
  , ErrorT(..)
  , mkError
  ) where

import Text.Printf (printf)

class (Show a) =>
      ErrorT a where
  kind :: a -> String
  report :: a -> IO ()
  report err = printf "%s: %s\n" (kind err) (show err)

data Error where
  Error :: (ErrorT a) => a -> Error

instance Show Error where
  show (Error a) = show a

instance ErrorT Error where
  kind (Error a) = kind a

mkError :: (ErrorT a) => a -> Either Error b
mkError a = Left (Error a)
