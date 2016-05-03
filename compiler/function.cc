#include "function.h"

#include "vm.h"

namespace ceos {

  std::string Function::name(VM *vm) {
    return vm->m_stringTable[id];
  }

}
