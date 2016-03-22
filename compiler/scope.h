#include <iostream>
#include <memory>
#include <unordered_map>

#ifndef CEOS_SCOPE_H
#define CEOS_SCOPE_H

namespace ceos {

  template<typename T>
  class Scope : public std::enable_shared_from_this<Scope<T>> {
    typedef std::shared_ptr<Scope<T>> ScopePtr;

    public:
      Scope() {}

      inline ScopePtr create() {
        auto s = std::make_shared<Scope<T>>();
        s->parent = this->shared_from_this();
        return s;
      }

      inline ScopePtr create(ScopePtr parent) {
        auto s = std::make_shared<Scope<T>>();
        s->parent = parent;
        s->previous = this->parent;
        return s;
      }

      inline ScopePtr restore() {
        return previous ?: parent ?: this->shared_from_this();
      }

      inline T get(std::string &var) {
        auto it = table.find(var);
        if (it != table.end()) return it->second;
        else if (parent) return parent->get(var);
        else return T();
      }

      inline void set(std::string key, T value) {
        table[key] = value;
      }

    private:
      ScopePtr parent;
      ScopePtr previous;
      std::unordered_map<std::string, T> table;
  };

}

#endif
