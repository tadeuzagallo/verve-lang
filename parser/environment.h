#include <memory>
#include <unordered_map>
#include <string>

#pragma once

namespace Verve {
  struct Environment;
  struct Type;
  namespace AST {
    struct NodeInterface;
  }

  using EnvPtr = std::shared_ptr<Environment>;

  struct Environment : public std::enable_shared_from_this<Environment> {
    EnvPtr create() {
      auto env = std::make_shared<Environment>();
      env->m_parent = shared_from_this();
      return env;
    }

    struct Entry {
      Type *type = nullptr;
      AST::NodeInterface *node = nullptr;
    };

    const Entry &get(std::string name) const {
      auto env = this;
      while (env) {
        auto it = env->m_entries.find(name);
        if (it != env->m_entries.end()) {
          return it->second;
        }
        env = env->m_parent.get();
      }
      static Entry dummy;
      dummy.type = nullptr;
      dummy.node = nullptr;
      return dummy;
    }

    EnvPtr envFor(const std::string &name) {
      auto env = this->shared_from_this();
      do {
        auto it = env->m_entries.find(name);
        if (it != env->m_entries.end()) {
          return env;
        }
      } while ((env = env->parent()) != nullptr);
      return nullptr;
    }

    Entry &create(std::string name) {
      return m_entries[name];
    }

    const std::unordered_map<std::string, Entry> &entries() const {
      return m_entries;
    }

    const EnvPtr parent() const {
      return m_parent;
    }

    bool isRequired = false;
    bool capturesScope = false;
    bool escapes = false;

    static std::unordered_map<std::string, std::string> reverseGenericMapping;
  private:
    std::unordered_map<std::string, Entry> m_entries;
    EnvPtr m_parent = nullptr;
  };
}
