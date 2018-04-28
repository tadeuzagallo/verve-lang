{-# LANGUAGE CPP #-}

module Absyn.Typed
  ( module Absyn.Meta
  , module Absyn.Typed
  , module Absyn.Base
  ) where

import Absyn.Meta
import Absyn.Base

import Typing.Types (Type)

type Id = (String, Type)

#define METADATA ()
#define NAME Id
#include "./Base.h"
