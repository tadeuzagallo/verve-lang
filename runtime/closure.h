#include "function.h"
#include "scope.h"
#include "value.h"

#pragma once

namespace ceos {

  struct Closure {
    Scope *scope;
    Function *fn;

    Closure() : scope(NULL) {}
    Closure(Scope *s) : scope(s->inc()) {}
    ~Closure() { if (scope) scope->dec(); }
  };

}
