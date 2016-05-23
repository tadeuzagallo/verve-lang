#include "type.h"

#pragma once

namespace ceos {
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
    std::shared_ptr<Environment> parent;
  };
}

