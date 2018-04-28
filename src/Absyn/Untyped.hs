{-# LANGUAGE CPP #-}

module Absyn.Untyped
  ( module Absyn.Meta
  , module Absyn.Untyped
  , module Absyn.Base
  , module Absyn.Type
  ) where

import Absyn.Base
import Absyn.Meta
import Absyn.Type

#define METADATA ()
#define NAME String
#include "./Base.h"
