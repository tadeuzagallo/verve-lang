#include "function.h"
#include "scope.h"
#include "value.h"

#ifndef CEOS_CLOSURE_H
#define CEOS_CLOSURE_H

namespace ceos {

  struct Closure {
    Scope *scope;
    Function *fn;

    Closure() : scope(NULL) {}
    Closure(Scope *s) : scope(s->inc()) {}
    ~Closure() { if (scope) scope->dec(); }
  };

}

#endif
