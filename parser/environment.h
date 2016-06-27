#include <memory>
#include <unordered_map>
#include <string>

#pragma once

namespace ceos {
  struct Environment;
  struct Type;

  typedef std::shared_ptr<Environment> EnvPtr;

  struct Environment {
    Type *get(std::string typeName) {
      auto env = this;
      while (env) {
        auto it = env->types.find(typeName);
        if (it != env->types.end()) {
          return it->second;
        }
        env = env->parent.get();
      }
      return nullptr;
    }

    std::unordered_map<std::string, Type *> types;
    EnvPtr parent;
  };
}

