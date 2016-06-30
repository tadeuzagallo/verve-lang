#include "gc.h"

namespace verve {

  std::set<uint64_t> GC::roots;
  std::set<Scope *> GC::scopes;

}
