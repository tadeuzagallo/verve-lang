#include "gc.h"

namespace Verve {

  std::set<uint64_t> GC::roots;
  std::set<Scope *> GC::scopes;

}
