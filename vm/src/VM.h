#pragma once

#include "Bytecode.h"

#include <cstdint>

namespace Verve {

class VM {
public:
  VM(Bytecode &&bc);

  uint32_t ip;
  Block* block;
  Bytecode bytecode;
};

}
