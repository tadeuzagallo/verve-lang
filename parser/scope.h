#include "parser/ast.h"

#include <functional>
#include <iostream>
#include <memory>
#include <unordered_map>

#pragma once

namespace ceos {
  class ParseScope;

  typedef std::shared_ptr<ParseScope> ParseScopePtr;

  class ParseScope : public std::enable_shared_from_this<ParseScope> {
    public:

      inline ParseScopePtr create() {
        auto s = std::make_shared<ParseScope>();
        s->m_parent = this->shared_from_this();
        return s;
      }

      inline ParseScopePtr restore() {
        return m_parent;
      }

      inline void set(std::string key, AST::NodePtr value) {
        m_table[key] = value;
      }

      inline AST::NodePtr get(std::string &var, bool recursive = true) {
        auto it = m_table.find(var);
        if (it != m_table.end()) return it->second;
        else if (recursive && m_parent != nullptr) return m_parent->get(var);
        else return nullptr;
      }

      inline bool isInCurrentParseScope(std::string &var) {
        auto it = m_table.find(var);
        return it != m_table.end();
      }

      ParseScopePtr scopeFor(std::string &var) {
        auto scope = this->shared_from_this();
        do {
          if (scope->isInCurrentParseScope(var)) { return scope; }
        } while ((scope = scope->parent()) != nullptr);
        return nullptr;
      }

      inline void visit(std::function<void(AST::NodePtr)> visitor) {
        ParseScopePtr ptr = this->shared_from_this();
        for (auto it : m_table) {
          visitor(it.second);
        }
      }

      inline ParseScopePtr &parent() {
        return m_parent;
      }

      inline unsigned stackSlot() {
        return stackSlotCount++;
      }

      bool isRequired;
      bool capturesScope;
      bool escapes = true;
      unsigned stackSlotCount = 0;
    private:
      ParseScopePtr m_parent;
      std::unordered_map<std::string, AST::NodePtr> m_table;
  };
}
