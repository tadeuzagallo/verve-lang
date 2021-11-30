#pragma once

#include "Bytecode.h"

#include <fstream>

namespace Verve {

class Decoder {
public:
  Decoder(std::ifstream&&);
  ~Decoder();

  Bytecode decode();

private:
  std::ifstream m_is;
};

}
