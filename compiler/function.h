#include <iostream>
#include <vector>

#ifndef CEOS_FUNCTION_H
#define CEOS_FUNCTION_H

namespace ceos {
  class VM;

  struct Function {
    Function(unsigned i, unsigned args, unsigned o, std::vector<char *> &&a) :
      id(i),
      offset(o),
      nargs(args),
      args(a) {}

    std::string name(VM *);

    unsigned id;
    unsigned offset;
    unsigned nargs;
    std::vector<char *> args;
  };

}

#endif
