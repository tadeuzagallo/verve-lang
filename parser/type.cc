#include "type.h"

#include "environment.h"

namespace Verve {

  bool BasicType::accepts(Type *other, __unused EnvPtr _) {
    if (auto t = dynamic_cast<BasicType *>(other)) {
      return typeName == t->typeName;
    }
    return false;
  }

  bool GenericType::accepts(Type *other, EnvPtr env) {
    if (auto t = env->get(typeName).type) {
      if (t != this) {
        return t->accepts(other, env);
      } else {
        env->create(typeName).type = other;
        return true;
      }
    }
    if (auto gt = dynamic_cast<GenericType *>(other)) {
      return typeName == gt->typeName;
    }
    return false;
  }

  bool DataTypeInstance::accepts(Type *other, EnvPtr env) {
    DataTypeInstance *t;
    if (!(t = dynamic_cast<DataTypeInstance *>(other))) {
      return false;
    }
    if (!dataType->accepts(t->dataType, env)) {
      return false;
    }
    if (types.size() != t->types.size()) {
      return false;
    }
    for (unsigned i = 0; i < types.size(); i++) {
      if (t->types[i] && !types[i]->accepts(t->types[i], env) && !t->types[i]->accepts(types[i], env)) {
        return false;
      }
    }
    return true;
  }

  bool TypeFunction::accepts(Type *other, EnvPtr env) {
    TypeFunction *t;
    if (!(t = dynamic_cast<TypeFunction *>(other))) {
      return false;
    }
    for (unsigned i = 0; i < types.size(); i++) {
      if (!types[i]->accepts(t->types[i], env)) {
        return false;
      }
    }
    if (!returnType->accepts(t->returnType, env)) {
      return false;
    }
    return true;
  }

  bool TypeInterface::accepts(Type *other, EnvPtr env) {
    if (other == this) {
      return true;
    }

    for (auto impl : implementations) {
      if (impl->type->accepts(other, env)) {
        env->create(genericTypeName).type = other;
        return true;
      }
    }

    return false;
  }

  bool TypeConstructor::accepts(__unused Type *other, __unused EnvPtr env) {
    return false;
  }

  bool EnumType::accepts(Type *other, __unused EnvPtr env) {
    if (auto ctor = dynamic_cast<TypeConstructor *>(other)) {
      return ctor->returnType == this;
    }
    return other == this;
  }

}
