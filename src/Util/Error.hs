{-# LANGUAGE FlexibleContexts #-}
module Util.Error
  ( Error(..)
  , ErrorT(..)
  , GenericError(..)
  , Result
  , liftParseError
  , throwError
  ) where

import Text.Parsec (ParseError)
import Text.Printf (printf)

import qualified Control.Monad.Except as Except (MonadError, throwError)

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

data GenericError = GenericError String

instance Show GenericError where
  show (GenericError s) = s

instance ErrorT GenericError where
  kind _ = "Error"

liftParseError :: Either ParseError b -> Either Error b
liftParseError (Left err) =
   Left (Error err)
liftParseError (Right b) = Right b

throwError :: (Except.MonadError Error m, ErrorT a) => a -> m b
throwError = Except.throwError . Error
