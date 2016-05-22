#include <functional>
#include <iostream>
#include <memory>
#include <unordered_map>

#pragma once

namespace ceos {

  template<typename T>
  class OldScope : public std::enable_shared_from_this<OldScope<T>> {
    typedef std::shared_ptr<OldScope<T>> OldScopePtr;

    public:
      OldScope() {}

      inline OldScopePtr create() {
        auto s = std::make_shared<OldScope<T>>();
        s->m_parent = this->shared_from_this();
        return s;
      }

      inline OldScopePtr create(OldScopePtr parent) {
        auto s = std::make_shared<OldScope<T>>();
        s->m_parent = parent;
        s->m_previous = this->shared_from_this();
        return s;
      }

      inline OldScopePtr restore() {
        return m_previous ?: m_parent ?: this->shared_from_this();
      }

      inline void set(std::string key, T value) {
        m_table[key] = value;
      }

      inline T get(std::string &var, bool recursive = true) {
        auto it = m_table.find(var);
        if (it != m_table.end()) return it->second;
        else if (recursive && m_parent != nullptr) return m_parent->get(var);
        else return nullptr;
      }

      inline bool isInCurrentScope(std::string &var) {
        auto it = m_table.find(var);
        return it != m_table.end();
      }

      OldScopePtr scopeFor(std::string &var) {
        auto scope = this->shared_from_this();
        do {
          if (scope->isInCurrentScope(var)) { return scope; }
        } while ((scope = scope->parent()) != nullptr);
        return nullptr;
      }

      inline void visit(std::function<void(T)> visitor) {
        OldScopePtr ptr = this->shared_from_this();
        for (auto it : m_table) {
          visitor(it.second);
        }
      }

      inline OldScopePtr &parent() {
        return m_parent;
      }

      bool isRequired;
      bool capturesScope;
    private:
      OldScopePtr m_parent;
      OldScopePtr m_previous;
      std::unordered_map<std::string, T> m_table;
  };

}
