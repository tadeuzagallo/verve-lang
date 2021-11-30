module Bytecode.Encoder
  ( encodeBytecode
  ) where

import Absyn.Base (Literal(..))
import Bytecode.Opcodes

import Data.Bits ((.|.), shiftL)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder, doubleLE, char8, stringUtf8, toLazyByteString)
import qualified Data.ByteString.Builder as Builder (int8, int32LE)

-- TODO: check for overflow
int8 :: Integral i => i -> Builder
int8 = Builder.int8 . fromInteger . toInteger

int32LE :: Integral i => i -> Builder
int32LE = Builder.int32LE . fromInteger . toInteger

encodeBytecode :: Bytecode -> ByteString
encodeBytecode = toLazyByteString . encode

class Encoder t where
  encode :: t -> Builder

instance Encoder t => Encoder [t] where
  encode ts = mconcat $ [encode t | t <- ts]

instance Encoder Bytecode where
  encode (Bytecode blocks) =
    encode blocks

instance Encoder Block where
  encode (Block _label _id _parameters locals constants code) =
    int32LE locals `mappend`
    int32LE (length code) `mappend`
    encode code `mappend`
    int32LE (length constants) `mappend`
    encode constants

instance Encoder Opcode where
  encode (OpCall callee numArgs) =
    int8 0 `mappend`
    encode callee `mappend`
    encode numArgs
  encode (OpPushReg r) =
    int8 1 `mappend`
    encode r
  encode (OpPushConst r) =
    int8 2 `mappend`
    encode r
  encode (OpLoadConst src dst) =
    int8 3 `mappend`
    encode src `mappend`
    encode dst
  encode OpError =
    int8 4
  encode (OpJumpCase val) =
    int8 5 `mappend`
    encode val
  encode (OpMakeTaggedValue dst tag numArgs) =
    int8 6 `mappend`
    encode dst `mappend`
    encode tag `mappend`
    encode numArgs
  encode (OpMakeClosure dst block) =
    int8 7 `mappend`
    encode dst `mappend`
    encode block
  encode (OpMakeRecord dst numFields) =
    int8 8 `mappend`
    encode dst `mappend`
    encode numFields
  encode (OpMakeType dst typeId) =
    int8 9 `mappend`
    encode dst `mappend`
    encode typeId

instance Encoder Register where
  encode (Parameter p) = -- 0b0...0
    int8 (p `shiftL` 1)
  encode (Local l) =     -- 0b0...1
    int8 ((l `shiftL` 1) .|. 1)

instance Encoder Const where
  encode (Constant c) =  -- 0b0...10
    int8 c

instance Encoder BlockRef where
  encode (BlockRef b) =  -- 0b0...11
    int8 b

instance Encoder Constant where
  encode (Unit) =
    int8 0
  encode (Literal (Integer i)) =
    int8 1 `mappend`
    int32LE i
  encode (Literal (String s)) =
    int8 2 `mappend`
    int8 (length s) `mappend`
    stringUtf8 s
  encode (Literal (Float f)) =
    int8 3 `mappend`
    doubleLE f
  encode (Literal (Char c)) =
    int8 4 `mappend`
    char8 c
