#include <functional>
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
        s->m_parent = this->shared_from_this();
        return s;
      }

      inline ScopePtr create(ScopePtr m_parent) {
        auto s = std::make_shared<Scope<T>>();
        s->m_parent = m_parent;
        s->m_previous = this->shared_from_this();
        return s;
      }

      inline ScopePtr restore() {
        return m_previous ?: m_parent ?: this->shared_from_this();
      }

      inline T get(std::string &var) {
        auto it = m_table.find(var);
        if (it != m_table.end()) return it->second;
        else if (m_parent) return m_parent->get(var);
        else return T();
      }

      inline void set(std::string key, T value) {
        m_table[key] = value;
      }

      inline void visit(std::function<void(T)> visitor) {
        ScopePtr ptr = this->shared_from_this();
        for (auto it : m_table) {
          visitor(it.second);
        }
      }

      inline ScopePtr &parent() {
        return m_parent;
      }

    private:
      ScopePtr m_parent;
      ScopePtr m_previous;
      std::unordered_map<std::string, T> m_table;
  };

}

#endif
