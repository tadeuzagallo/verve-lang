module Bytecode.Opcodes where

import Prelude hiding (concat)

import Absyn.Base (Literal(..))
import Util.PrettyPrint

data Operand
  = Local Int
  | Parameter Int
  | Constant Int
  | BlockRef Int
  deriving (Show)

data Opcode
  = OpReturn { opReturnResult :: Operand }
  | OpCall { opCallCallee :: Operand
           , opCallNumArgs :: Operand }
  | OpPush { opPushValue :: Operand }
  | OpJump { opJumpTarget :: Operand }
  | OpMove { opMoveSrc :: Operand
           , opMoveDst :: Operand }
  | OpError
  | OpJumpCase { opJumpCaseValue :: Operand }
  | OpMakeTaggedValue { opMakeTaggedValueDst :: Operand
                      , opMakeTaggedValueKey :: Operand
                      , opMakeTaggedValueNumArgs :: Operand }
  | OpMakeClosure { opMakeClosureDst :: Operand
                  , opMakeClosureBlockId :: Operand }
  | OpMakeRecord { opMakeRecordDst :: Operand
                 , opMakeRecordNumFields :: Operand }
  | OpMakeType { opMakeTypeDst :: Operand
               , opMakeTypeId :: Operand }

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
  pprint (OpReturn r) =
    pprintInstruction "return" [r]
  pprint (OpCall callee numArgs) =
    concat [ str "call ", pprint callee, str " (", str (show numArgs), str ")" ]
  pprint (OpPush r) =
    pprintInstruction "push" [r]
  pprint (OpJump r) =
    concat [ str "jump ", pprint r ]
  pprint (OpMove src dst) =
    pprintInstruction "move" [src, dst]
  pprint OpError =
    pprintInstruction "error" []
  pprint (OpJumpCase val) =
    concat [str "jump_case ", pprint val ]
  pprint (OpMakeTaggedValue dst tag numArgs) =
    concat [pprint dst, str " = make_tagged_value ", pprint tag, str " ", str (show numArgs)]
  pprint (OpMakeClosure dst block) =
    concat [pprint dst, str " = make_closure ", pprint block]
  pprint (OpMakeRecord dst numFields) =
    concat [pprint dst, str " = make_record ", str (show numFields)]
  pprint (OpMakeType dst typeId) =
    concat [pprint dst, str " = make_type ", pprint typeId]

pprintInstruction :: String -> [Operand] -> Out
pprintInstruction name operands =
  concat [str name, str " ", interleave (str ", ") $ map pprint operands]

instance PrettyPrint Operand where
  pprint (Local i) =
    concat [str "l", str $ show i]
  pprint (Parameter i) =
    concat [str "p", str $ show i]
  pprint (Constant i) =
    concat [str "c", str $ show i]
  pprint (BlockRef i) =
    concat [str "b", str $ show i]

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
