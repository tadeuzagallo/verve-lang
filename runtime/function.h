#include <iostream>
#include <vector>

#include "ceos_string.h"

#pragma once

namespace ceos {
  class VM;

  struct Function {
    Function(unsigned i, unsigned args, unsigned o, std::vector<String> &&a) :
      id(i),
      offset(o),
      nargs(args),
      args(a) {}

    String name(VM *);

    unsigned id;
    unsigned offset;
    unsigned nargs;
    std::vector<String> args;
  };

}
