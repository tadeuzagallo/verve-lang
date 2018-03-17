{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Interpreter.Builtin where

import Absyn.Meta
import Interpreter.Value

import Data.Char (ord)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

int_add :: Value
int_add =
  VBuiltin
    (\(VLit (Integer a)) ->
       VBuiltin $ \(VLit (Integer b)) ->
         VLit . Integer $ a + b)

int_sub :: Value
int_sub =
  VBuiltin (\(VLit (Integer a)) ->
    VBuiltin $ \(VLit (Integer b)) ->
      VLit . Integer $ a - b)

int_mul :: Value
int_mul =
  VBuiltin (\(VLit (Integer a)) ->
    VBuiltin $ \(VLit (Integer b)) ->
      VLit . Integer $ a * b)

int_div :: Value
int_div =
  VBuiltin (\(VLit (Integer a)) ->
    VBuiltin $ \(VLit (Integer b)) ->
      VLit . Integer $ a `div` b)

int_mod :: Value
int_mod =
  VBuiltin (\(VLit (Integer a)) ->
    VBuiltin $ \(VLit (Integer b)) ->
      VLit . Integer $ a `mod` b)

int_neg :: Value
int_neg =
  VBuiltin (\(VLit (Integer a)) ->
    VLit . Integer $ negate a)

int_lt :: Value
int_lt =
  VBuiltin
    (\(VLit (Integer a)) ->
       VBuiltin $ \(VLit (Integer b)) ->
         if a < b then VIn "True" [] else VIn "False" [])

int_gt :: Value
int_gt =
  VBuiltin
    (\(VLit (Integer a)) ->
       VBuiltin $ \(VLit (Integer b)) ->
         if a > b then VIn "True" [] else VIn "False" [])

int_to_string :: Value
int_to_string =
  VBuiltin (\(VLit (Integer a)) ->
    VLit . String $ show a)

string_print :: Value
string_print =
  VBuiltin (\v ->
    case unsafePerformIO (print v) of
      () -> VUnit)

fieldAccess :: Value
fieldAccess =
  VBuiltin (\(VLit (String field)) ->
    VBuiltin $ \(VRecord fields) ->
      fromJust $ lookup field fields)

{-unwrapClass :: Value-}
{-unwrapClass =-}
  {-VBuiltin (\(VNeutral (NApp _ v)) -> v)-}

strlen :: Value
strlen =
  VBuiltin $ \(VLit (String field)) ->
    VLit . Integer . toInteger $ length field

substr :: Value
substr =
  VBuiltin $ \(VLit (String str)) ->
    VBuiltin $ \(VLit (Integer off)) ->
      VBuiltin $ \(VLit (Integer len)) ->
        VLit . String . take (fromInteger len) . drop (fromInteger off) $ str

charAt :: Value
charAt =
  VBuiltin $ \(VLit (String str)) ->
    VBuiltin $ \(VLit (Integer index)) ->
      VLit . Char $ str !! fromInteger index

char_to_int :: Value
char_to_int =
  VBuiltin $ \(VLit (Char c)) ->
    VLit . Integer . toInteger $ ord c

read_file :: Value
read_file =
  VBuiltin $ \(VLit (String fileName)) ->
    case unsafePerformIO (readFile fileName) of
      content ->
        VLit . String $ content

sqrt' :: Value
sqrt' =
  VBuiltin $ \(VLit (Float f)) ->
    VLit . Float $ sqrt f

ceil' :: Value
ceil' =
  VBuiltin $ \(VLit (Float f)) ->
    VLit . Integer $ ceiling f
