#include "type.h"

#include "environment.h"
#include "type_helpers.h"

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
        return typeEq(t, other, env);
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
    if (!typeEq(dataType, t->dataType, env)) {
      return false;
    }
    if (types.size() != t->types.size()) {
      return false;
    }
    for (unsigned i = 0; i < types.size(); i++) {
      // if `other` has a null type, that means it's a data type that doesn't
      // use the type variable, which means it can accept any type.
      if (t->types[i] && !typeEq(types[i], t->types[i], env) && !typeEq(t->types[i], types[i], env)) {
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
    env = env->create();
    loadGenerics(t->generics, env);
    for (unsigned i = 0; i < types.size(); i++) {
      if (!typeEq(t->types[i], types[i], env)) {
        return false;
      }
    }
    if (!typeEq(returnType, t->returnType, env)) {
      return false;
    }
    return true;
  }

  bool TypeInterface::accepts(Type *other, EnvPtr env) {
    if (other == this) {
      return true;
    }

    for (auto impl : implementations) {
      if (typeEq(impl->type, other, env)) {
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
