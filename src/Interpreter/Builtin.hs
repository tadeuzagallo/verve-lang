{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Interpreter.Builtin where

import Absyn.Meta
import Interpreter.Value

import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

int_add :: Value
int_add =
  VLam
    (\(VLit (Integer a)) ->
       return . VLam $ \(VLit (Integer b)) ->
         return . VLit . Integer $ a + b)

int_sub :: Value
int_sub =
  VLam (\(VLit (Integer a)) ->
    return . VLam $ \(VLit (Integer b)) ->
      return . VLit . Integer $ a - b)

int_mul :: Value
int_mul =
  VLam (\(VLit (Integer a)) ->
    return . VLam $ \(VLit (Integer b)) ->
      return . VLit . Integer $ a * b)

int_div :: Value
int_div =
  VLam (\(VLit (Integer a)) ->
    return . VLam $ \(VLit (Integer b)) ->
      return . VLit . Integer $ a `div` b)

int_neg :: Value
int_neg =
  VLam (\(VLit (Integer a)) ->
    return . VLit . Integer $ negate a)

string_print :: Value
string_print =
  VLam (\v ->
    case unsafePerformIO (print v) of
      () -> return VVoid)

fieldAccess :: Value
fieldAccess =
  VLam (\(VLit (String field)) ->
    return . VLam $ \(VRecord fields) ->
      return . fromJust $ lookup field fields)

unwrapClass :: Value
unwrapClass =
  VLam (\(VNeutral (NApp _ v)) ->
    return v)
