#include "function.h"

#include "vm.h"

namespace Verve {

  String Function::name(VM *vm) {
    return vm->m_stringTable[id];
  }

}
