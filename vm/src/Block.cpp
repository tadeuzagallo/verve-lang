#include "Block.h"

#include "VM.h"

namespace Verve {

template<>
Opcode Block::read(VM* vm)
{
    return static_cast<Opcode>(m_code[vm->ip++]);
}

}
