#include "macros.h"

#ifndef CEOS_OPCODES_H
#define CEOS_OPCODES_H

class Opcode {
  public:
    ENUM(Type,
      ret,
      push,
      call,
      jz,
      jmp,
      load_string,
      push_arg,
      lookup
    );
};

#endif
