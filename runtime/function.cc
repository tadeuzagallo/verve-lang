#include "function.h"

#include "vm.h"

namespace verve {

  String Function::name(VM *vm) {
    return vm->m_stringTable[id];
  }

}
