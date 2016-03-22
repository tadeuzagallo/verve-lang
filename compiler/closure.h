#include "function.h"
#include "scope.h"
#include "value.h"

#ifndef CEOS_CLOSURE_H
#define CEOS_CLOSURE_H

namespace ceos {

  struct Closure {
    Function *fn;
    std::shared_ptr<Scope<Value>> scope;
  };

}

#endif
