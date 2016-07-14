#include <memory>
#include <unordered_map>
#include <string>

#pragma once

namespace Verve {
  struct Environment;
  struct Type;

  typedef std::shared_ptr<Environment> EnvPtr;

  struct Environment : public std::enable_shared_from_this<Environment> {
    Type *get(std::string typeName) {
      auto env = this;
      while (env) {
        auto it = env->m_types.find(typeName);
        if (it != env->m_types.end()) {
          return it->second;
        }
        env = env->parent.get();
      }
      return nullptr;
    }

    EnvPtr create() {
      auto env = std::make_shared<Environment>();
      env->parent = shared_from_this();
      return env;
    }

    template<typename T>
    void set(T &&name, Type *type) {
      m_types[std::forward<T>(name)] = type;
    }

    const std::unordered_map<std::string, Type *> &types() const {
      return m_types;
    }

  private:
    std::unordered_map<std::string, Type *> m_types;
    EnvPtr parent = nullptr;
  };
}

