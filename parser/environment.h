#include <memory>
#include <unordered_map>
#include <string>

#pragma once

namespace verve {
  struct Environment;
  struct Type;

  typedef std::shared_ptr<Environment> EnvPtr;

  struct Environment : public std::enable_shared_from_this<Environment> {
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

    EnvPtr create() {
      auto env = std::make_shared<Environment>();
      env->parent = shared_from_this();
      return env;
    }

    std::unordered_map<std::string, Type *> types;
    EnvPtr parent;
  };
}

