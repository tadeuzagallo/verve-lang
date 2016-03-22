#include "function.h"
#include "scope.h"

#ifndef CEOS_CLOSURE_H
#define CEOS_CLOSURE_H

namespace ceos {

  struct Closure {
    Function *fn;
    std::shared_ptr<Scope> scope;
  };

}

#endif
