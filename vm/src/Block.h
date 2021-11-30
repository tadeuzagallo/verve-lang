#pragma once

#include "Opcodes.h"
#include "Value.h"

#include <cstdint>
#include <vector>

namespace Verve {

class VM;

class Block {
public:
  template<class T>
  T read(VM*);

private:
  uint32_t m_locals;
  std::vector<char> m_code;
  std::vector<Value> m_constants;
};

template<>
Opcode Block::read(VM*);

}
