#include "type_helpers.h"

namespace Verve {

std::string uniqueName(const std::string &name, EnvPtr env) {
  static int id = 0;
  auto newName = "T" + std::to_string(id++);
  env->create(name).type = new GenericType(newName);
  Environment::reverseGenericMapping[newName] = name;
  return newName;
}

std::string generic(const std::string &name, EnvPtr env) {
  if (auto generic = dynamic_cast<GenericType *>(env->get(name).type)) {
    return generic->typeName;
  }
  return name;
}

void loadGenerics(const std::vector<std::string> &generics, EnvPtr env) {
  for (auto &g : generics) {
    env->create(g).type = new GenericType(g);
  }
}

Type *simplify(Type *type, EnvPtr env) {
  if (auto gt = dynamic_cast<GenericType *>(type)) {
    auto t = env->get(gt->typeName).type;
    if (t && t != type) {
      return simplify(t, env);
    }
  } else if (auto dti = dynamic_cast<DataTypeInstance *>(type)) {
    auto new_dti = new DataTypeInstance(*dti);
    new_dti->dataType = simplify(dti->dataType, env);
    for (unsigned i = 0; i < dti->types.size(); i++) {
      new_dti->types[i] = simplify(dti->types[i], env);
    }
    return new_dti;
  } else if (auto interface = dynamic_cast<TypeInterface *>(type)) {
    auto t = env->get(interface->genericTypeName).type;
    if (t && t != type && !dynamic_cast<GenericType *>(t)) {
      return simplify(t, env);
    }
  } else if (auto fn = dynamic_cast<TypeFunction *>(type)) {
    auto new_fn = new TypeFunction(*fn);
    for (unsigned i = 0; i < fn->types.size(); i++) {
      new_fn->types[i] = simplify(fn->types[i], env);
    }
    new_fn->returnType = simplify(fn->returnType, env);
    return new_fn;
  }
  return type;
}

bool typeEq(Type *expected, Type *actual, EnvPtr env) {
  return simplify(expected, env)->accepts(simplify(actual, env), env);
}

Type *enumRetType(const TypeFunction *fnType, EnvPtr env) {
  if (auto et = dynamic_cast<EnumType *>(fnType->returnType)) {
    if (et->generics.size()) {
      auto returnType = new DataTypeInstance();
      returnType->dataType = et;
      for (auto t : et->generics) {
        returnType->types.push_back(env->get(t).type);
      }
      return returnType;
    }
  }
  return simplify(fnType->returnType, env);
}

Type *typeCheckArguments(const std::vector<AST::NodePtr> &arguments, const TypeFunction *fnType, EnvPtr env, const Loc &loc) {
  if (arguments.size() != fnType->types.size()) {
    throw TypeError(loc, "Wrong number of arguments for function call");
  }

  loadGenerics(fnType->generics, env);

  for (unsigned i = 0; i < fnType->types.size(); i++) {
    auto arg = arguments[i];

    auto expected = fnType->types[i];
    auto actual = arg->typeof(env);

    if (!actual) {
      throw TypeError(arg->loc, "Can't find type information for call argument #%d", i + 1);
    } else if (!typeEq(expected, actual, env->create())) {
      throw TypeError(arg->loc, "Expected `%s` but got `%s` on arg #%d for function `%s`", expected->toString().c_str(), actual->toString().c_str(), i + 1, fnType->name.c_str());
    }
  }

  return enumRetType(fnType, env);
}

}
