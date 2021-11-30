#include "Interpreter.h"

#include "Block.h"
#include "VM.h"

namespace Verve {

Interpreter::Interpreter(VM& vm)
  : m_vm(vm)
{}

void Interpreter::run()
{
    Opcode op = m_vm.block->read<Opcode>(&m_vm);
}

}
