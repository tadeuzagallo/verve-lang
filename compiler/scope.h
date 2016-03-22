#include "value.h"

#include <memory>
#include <unordered_map>

#ifndef CEOS_SCOPE_H
#define CEOS_SCOPE_H

namespace ceos {

  struct Scope {
    Scope(std::shared_ptr<Scope> p) : parent(p) { }

    Scope(std::shared_ptr<Scope> p, std::shared_ptr<Scope> o) : parent(p) , other(o) { }

    Value get(std::string &var) {
      std::unordered_map<std::string, Value>::iterator v;
      if ((v = table.find(var)) != table.end()) return v->second;
      else if (parent.get() != this) return parent->get(var);
      else {
        std::cerr << "Symbol not found: " << var << "\n";
        throw;
      }
    }

    std::shared_ptr<Scope> parent;
    std::shared_ptr<Scope> other;
    std::unordered_map<std::string, Value> table;
  };

}

#endif
