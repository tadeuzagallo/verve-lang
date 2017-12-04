{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Interpreter.Builtin where

import Absyn.Meta
import Interpreter.Value

import Data.Char (ord)
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

int_mod :: Value
int_mod =
  VLam (\(VLit (Integer a)) ->
    return . VLam $ \(VLit (Integer b)) ->
      return . VLit . Integer $ a `mod` b)

int_neg :: Value
int_neg =
  VLam (\(VLit (Integer a)) ->
    return . VLit . Integer $ negate a)

int_lt :: Value
int_lt =
  VLam
    (\(VLit (Integer a)) ->
       return . VLam $ \(VLit (Integer b)) ->
         return . VNeutral . NFree $ if a < b then "True" else "False")

int_to_string :: Value
int_to_string =
  VLam (\(VLit (Integer a)) ->
    return . VLit . String $ show a)

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

strlen :: Value
strlen =
  VLam $ \(VLit (String field)) ->
    return . VLit . Integer . toInteger $ length field

substr :: Value
substr =
  VLam $ \(VLit (String str)) ->
    return . VLam $ \(VLit (Integer off)) ->
      return . VLam $ \(VLit (Integer len)) ->
        return . VLit . String . take (fromInteger len) . drop (fromInteger off) $ str

charAt :: Value
charAt =
  VLam $ \(VLit (String str)) ->
    return . VLam $ \(VLit (Integer index)) ->
      return . VLit . Char $ str !! fromInteger index

char_to_int :: Value
char_to_int =
  VLam $ \(VLit (Char c)) ->
    return . VLit . Integer . toInteger $ ord c

read_file :: Value
read_file =
  VLam $ \(VLit (String fileName)) ->
    case unsafePerformIO (readFile fileName) of
      content ->
        return . VLit . String $ content
