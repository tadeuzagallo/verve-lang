#include <iostream>
#include <vector>

#ifndef CEOS_FUNCTION_H
#define CEOS_FUNCTION_H

namespace ceos {
  class VM;

  struct Function {
    Function(unsigned i, unsigned args, unsigned o, std::vector<std::string *> &&a) :
      offset(o),
      id(i),
      nargs(args),
      args(a) {}

    std::string &name(VM *);

    inline std::string &arg(unsigned i) { return *args[i]; }

    unsigned offset;
    unsigned id;
    unsigned nargs;
    std::vector<std::string *> args;
  };

}

#endif
