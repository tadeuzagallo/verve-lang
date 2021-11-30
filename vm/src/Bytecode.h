#pragma once

#include "Block.h"

#include <vector>

namespace Verve {

class Bytecode {
private:
  std::vector<Block> m_blocks;
};

}
