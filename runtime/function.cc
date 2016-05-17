#include "function.h"

#include "vm.h"

namespace ceos {

  String Function::name(VM *vm) {
    return vm->m_stringTable[id];
  }

}
