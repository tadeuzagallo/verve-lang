#pragma once

enum Opcode {
  OpReturn,
  OpCall,
  OpPush,
  OpJump,
  OpMove,
  OpError,
  OpJumpCase,
  OpMakeTaggedValue,
  OpMakeClosure,
  OpMakeRecord,
  OpMakeType,
};
