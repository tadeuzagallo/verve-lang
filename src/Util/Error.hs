{-# LANGUAGE GADTs #-}

module Util.Error
  ( Error(..)
  , ErrorT(..)
  , Result
  , mkError
  , liftError
  ) where

import Text.Parsec (ParseError)
import Text.Printf (printf)

class (Show a) =>
      ErrorT a where
  kind :: a -> String
  showError :: a -> String
  showError err = printf "%s: %s" (kind err) (show err)

  report :: a -> IO ()
  report err = putStrLn $ showError err

data Error where
  Error :: (ErrorT a) => a -> Error

type Result = Either Error

instance Show Error where
  show (Error a) = show a

instance ErrorT Error where
  kind (Error a) = kind a

-- Orphan instance for Parsec.ParseError
instance ErrorT ParseError where
  kind _ = "ParseError"

mkError :: (ErrorT a) => a -> Either Error b
mkError a = Left (Error a)

liftError :: (ErrorT a) => Either a b -> Either Error b
liftError (Left a) = Left (Error a)
liftError (Right b) = Right b
