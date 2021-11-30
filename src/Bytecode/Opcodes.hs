module Bytecode.Opcodes where

import Prelude hiding (concat)

import Absyn.Base (Literal(..))
import Util.PrettyPrint

data Register
  = Local Int
  | Parameter Int
  deriving (Show)

newtype Const = Constant Int
newtype BlockRef = BlockRef Int

data Opcode
  = OpCall { opCallCallee :: Register
           , opCallNumArgs :: Const }
  | OpPushReg { opPushReg :: Register }
  | OpPushConst { opPushConst :: Const }
  | OpLoadConst { opLoadConst :: Const
                 , opLoadConstDst :: Register }
  | OpError
  | OpJumpCase { opJumpCaseValue :: Register }
  | OpMakeTaggedValue { opMakeTaggedValueDst :: Register
                      , opMakeTaggedValueKey :: Const
                      , opMakeTaggedValueNumArgs :: Const }
  | OpMakeClosure { opMakeClosureDst :: Register
                  , opMakeClosureBlockId :: BlockRef }
  | OpMakeRecord { opMakeRecordDst :: Register
                 , opMakeRecordNumFields :: Const }
  | OpMakeType { opMakeTypeDst :: Register
               , opMakeTypeId :: Const }

data Block =
  Block { label :: String
        , id :: Int
        , parameters :: Int
        , locals :: Int
        , constants :: [Constant]
        , code :: [Opcode] }

data Bytecode
  = Bytecode { bcBlocks :: [Block] }

data Constant
  = Literal Literal
  | Unit

instance PrettyPrint Bytecode where
  pprint (Bytecode blocks) =
    interleave (concat [newline, newline]) $ map pprint blocks

instance PrettyPrint Block where
  pprint (Block label id parameters locals consts code) =
    concat [ indent $ concat [str $ show id, str ": ", str label, str "(", str $ show parameters, str ", ", str $ show locals, str "):", newline
                             , interleave newline $ map pprint code, newline
                             , newline
                             , indent $ concat [ str "CONSTANTS:", newline
                                               , interleave newline $ zipWith (\i c -> concat [str $ show i, str ": ", pprint c]) ([0..] :: [Int]) consts
                                               ]
                             ]
           ]

instance PrettyPrint Opcode where
  pprint (OpCall callee numArgs) =
    concat [ str "call ", pprint callee, str " (", pprint numArgs, str ")" ]
  pprint (OpPushReg r) =
    pprintInstruction "push" [r]
  pprint (OpPushConst r) =
    pprintInstruction "push" [r]
  pprint (OpLoadConst src dst) =
    pprintInstruction "move" [pprint src, pprint dst]
  pprint OpError =
    str "error"
  pprint (OpJumpCase val) =
    concat [str "jump_case ", pprint val ]
  pprint (OpMakeTaggedValue dst tag numArgs) =
    concat [pprint dst, str " = make_tagged_value ", pprint tag, str " ", pprint numArgs]
  pprint (OpMakeClosure dst block) =
    concat [pprint dst, str " = make_closure ", pprint block]
  pprint (OpMakeRecord dst numFields) =
    concat [pprint dst, str " = make_record ", pprint numFields]
  pprint (OpMakeType dst typeId) =
    concat [pprint dst, str " = make_type ", pprint typeId]

pprintInstruction :: PrettyPrint a => String -> [a] -> Out
pprintInstruction name operands =
  concat [str name, str " ", interleave (str ", ") $ map pprint operands]

instance PrettyPrint Register where
  pprint (Local i) =
    concat [str "l(", str $ show i, str ")"]
  pprint (Parameter i) =
    concat [str "p(", str $ show i, str ")"]

instance PrettyPrint Const where
  pprint (Constant i) =
    concat [str "c(", str $ show i, str ")"]

instance PrettyPrint BlockRef where
  pprint (BlockRef i) =
    concat [str "b(", str $ show i, str ")"]

instance PrettyPrint Constant where
  pprint (Unit) =
    concat [str "()"]
  pprint (Literal (Integer i)) =
    concat [str "Int(", str $ show i, str ")"]
  pprint (Literal (String i)) =
    concat [str "String(", str $ show i, str ")"]
  pprint (Literal (Float i)) =
    concat [str "Float(", str $ show i, str ")"]
  pprint (Literal (Char i)) =
    concat [str "Char(", str $ show i, str ")"]
