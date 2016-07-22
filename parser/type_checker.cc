#include "type_checker.h"

#include <algorithm>
#include <cstdio>

#include "environment.h"
#include "type_error.h"
#include "type_helpers.h"

namespace Verve {

void TypeChecker::check(AST::ProgramPtr program, EnvPtr env, Lexer &lexer) {
  try {
    auto type = program->typeof(env);
    if (!type) {
      throw TypeError(program->loc(), "Unknown type for program");
    }
  } catch (TypeError &ex) {
    fprintf(stderr, "Type Error: %s\n", ex.what());
    lexer.printSource(ex.loc());
    throw std::runtime_error("type error");
  }
}

// AST nodes typeof
namespace AST {

// program
Type *Program::typeof(EnvPtr env) {
  return body->typeof(env);
}

// base types
Type *String::typeof(EnvPtr env) {
  return env->get("string").type;
}

Type *Number::typeof(EnvPtr env) {
  return isFloat ? env->get("float").type : env->get("int").type;
}

// basic constructs

Type *Identifier::typeof(EnvPtr env) {
  if (auto t = env->get(name).type) {
    return t;
  }
  throw TypeError(loc(), "Unknown identifier: `%s`", name.c_str());
}

Type *List::typeof(EnvPtr env) {
  Type *t = nullptr;
  for (auto i : items) {
    auto iType = i->typeof(env);
    if (!t || typeEq(iType, t, env)) {
      t = iType;
      continue;
    }
    if (typeEq(t, iType, env))
      continue;
    throw TypeError(i->loc(), "Lists can't have mixed types: found an element of type `%s` when elements' inferred type was `%s`", iType->toString().c_str(), t->toString().c_str());
  }

  auto dti = new DataTypeInstance();
  dti->dataType = env->get("list").type;
  dti->types.push_back(t);
  return dti;
}

Type *Block::typeof(EnvPtr env) {
  auto t = env->get("void").type;
  for (auto n : nodes) {
    t = n->typeof(this->env);
  }
  return t;
}

// scoped expressions

Type *Let::typeof(__unused EnvPtr _) {
  for (auto a : assignments) {
    a->typeof(block->env);
  }

  return block->typeof(block->env);
}

Type *Assignment::typeof(__unused EnvPtr env) {
  auto t = value->typeof(env);
  switch (kind) {
    case Assignment::Pattern:
      // constructor will already put variables on scope
      left.pattern->typeof(env);
      break;
    case Assignment::Identifier:
      env->create(left.ident->name).type = t;
      break;
    default:
      // TODO: proper error for invalid lhs in assignment
      assert(false);
  }
  return t;
}

Type *If::typeof(EnvPtr env) {
  // TODO: assert condition has type bool
  auto ifType = ifBody->typeof(env);
  if (!elseBody)
    return ifType;
  auto elseType = elseBody->typeof(env);
  if (typeEq(ifType, elseType, env))
    return ifType;
  if (typeEq(elseType, ifType, env))
    return elseType;
  // TODO: print inferred type for each branch
  throw TypeError(loc(), "`if` and `else` branches evaluate to different types");
}

Type *Match::typeof(EnvPtr env) {
  if (cases.size() == 0) {
    throw TypeError(loc(), "Cannot have `match` expression with no cases");
  }

  value->typeof(env);
  Type *t = nullptr;
  for (auto c : cases) {
    auto cType = c->typeof(env->create());
    if (!t || typeEq(cType, t, env)) {
      t = cType;
      continue;
    }
    if (typeEq(t, cType, env))
      t = cType;
    throw TypeError(c->loc(), "Match can't have mixed types on its cases: found a case with type `%s` when previous cases' inferred type was `%s`", cType->toString().c_str(), t->toString().c_str());
  }
  return t;
}

Type *Case::typeof(__unused EnvPtr _) {
  pattern->typeof(body->env);
  return body->typeof(body->env);
}

Type *Pattern::typeof(EnvPtr env) {
  auto t = dynamic_cast<Verve::TypeConstructor *>(env->get(constructorName).type);
  if (!t) {
    throw TypeError(loc(), "Unknown constructor `%s` on pattern match", constructorName.c_str());
  }

  // TODO: proper error message
  assert(t->types.size() == values.size());

  auto valueType = value->typeof(env);
  auto new_env = env->create();
  if (auto vt = dynamic_cast<DataTypeInstance *>(valueType)) {
    for (unsigned i = 0; i < t->generics.size(); i++) {
      new_env->create(t->generics[i]).type = vt->types[i];
    }
  }
  
  auto rt = enumRetType(t, new_env);
  if (!typeEq(valueType, rt, new_env)) {
    throw TypeError(loc(), "Trying to pattern match value of type `%s` with constructor `%s`", valueType->toString().c_str(), t->toString().c_str());
  }

  tag = t->tag;
  for (unsigned i = 0; i < values.size(); i++) {
    env->get(values[i]->name).type = simplify(t->types[i], new_env);
  }

  return t;
}

Type *UnaryOperation::typeof(EnvPtr env) {
  // TODO: type check value's type
  return env->get("int").type;
}

Type *BinaryOperation::typeof(EnvPtr env) {
  auto intType = env->get("int").type;
  NodePtr failed = nullptr;
  Type *failedType = nullptr;
  if (!typeEq(intType, (failedType = lhs->typeof(env)), env))
    failed = lhs;
  else if (!typeEq(intType, (failedType = rhs->typeof(env)), env))
    failed = rhs;

  if (!failed)
    return intType;

  throw TypeError(failed->loc(), "Binary operations only accept `int`, but found `%s`", failedType->toString().c_str());
}

// type nodes

Type *BasicType::typeof(EnvPtr env) {
  if (auto t = env->get(generic(name, env)).type) {
    return t;
  }
  throw TypeError(loc(), "Unknown type: `%s`", name.c_str());
}

Type *DataType::typeof(EnvPtr env) {
  auto dataType = env->get(generic(name, env)).type;
  if (!dataType) {
    // TODO: this should be a BasicType instead of a strings
    throw TypeError(loc(), "Unknown type: `%s`", name.c_str());
  }
  auto t = new DataTypeInstance();
  t->dataType = dataType;
  for (auto p : params) {
    t->types.push_back(p->typeof(env));
  }
  return t;
}

static Verve::TypeConstructor *typeConstructor(TypeConstructorPtr ctor, Verve::EnumType *enumType, unsigned tag, EnvPtr env) {
  auto t = new Verve::TypeConstructor();
  t->name = ctor->name;
  t->tag = tag;
  t->returnType = enumType;
  for (auto type : ctor->types) {
    auto tt = type->typeof(env);
    t->types.push_back(tt);
    if (auto gt = dynamic_cast<GenericType *>(tt)) {
      t->generics.push_back(gt->typeName);
    }
  }
  return t;
}

Type *EnumType::typeof(EnvPtr env) {
  auto t = new Verve::EnumType();
  t->name = name;
  t->generics = generics;
  env->create(name).type = t;

  auto new_env = env->create();

  for (auto &g : t->generics) {
    g = uniqueName(g, new_env);
  }
  loadGenerics(t->generics, new_env);

  auto tag = 0u;
  for (auto c : constructors) {
    auto ctor = typeConstructor(c, t, tag++, new_env);
    t->constructors.push_back(ctor);
    env->create(c->name).type = ctor;
  }
  return t;
}

TypeInterface *s_interface = nullptr;
Type *Interface::typeof(EnvPtr env) {
  auto interface = new TypeInterface();
  interface->name = name;
  interface->genericTypeName = uniqueName(genericTypeName, this->env);
  interface->virtualFunctions = virtualFunctions;
  interface->concreteFunctions = concreteFunctions;

  env->create(name).type = interface;

  this->env->create(interface->genericTypeName).type = interface;

  s_interface = interface;
  for (const auto &fn : functions) {
    env->create(fn->getName()).type = fn->typeof(this->env->create());
  }
  s_interface = nullptr;

  return interface;
}

static std::string s_implementationName = "";
Type *Implementation::typeof(EnvPtr env) {
  auto interface = dynamic_cast<TypeInterface *>(env->get(interfaceName).type);

  // TODO: proper error message
  assert(interface);

  auto t = new TypeImplementation();
  t->type = type->typeof(env);
  t->interface = interface;

  interface->implementations.push_back(t);

  this->env->create(interface->genericTypeName).type = t->type;

  auto virtualFunctions = interface->virtualFunctions;
  s_implementationName = "$" + t->type->toString();
  for (auto &fn : functions) {
    auto it = std::find(virtualFunctions.begin(), virtualFunctions.end(), fn->getName());
    if (it != virtualFunctions.end()) {
      virtualFunctions.erase(it);
    } else if (std::find(interface->concreteFunctions.begin(), interface->concreteFunctions.end(), fn->getName()) == interface->concreteFunctions.end()) {
      throw TypeError(loc(), "Defining function `%s` inside implementation `%s`, but it's not part of the interface", fn->getName().c_str(), t->toString().c_str());
    }

    auto t = fn->typeof(this->env);
    env->create(fn->getName()).type = t;
    env->create(fn->getName()).node = fn.get();
  }
  s_implementationName = "";

  if (virtualFunctions.size() != 0) {
    auto index = 1;
    std::stringstream s;
    for (auto &fn : virtualFunctions) {
      s << index++ << ") " << fn << std::endl;
    }
    auto str = s.str();
    throw TypeError(loc(), "Implementation `%s` does not implement the following virtual functions:\n%s", t->toString().c_str(), str.c_str());
  }

  return env->get("void").type;
}

Type *Constructor::typeof(EnvPtr env) {
  auto type = env->get(name).type;
  env = env->create(); // isolate generic resolution
  if (auto ctorType = dynamic_cast<Verve::TypeConstructor *>(type)) {
    tag = ctorType->tag;
    size = ctorType->types.size() + 1;
    ctorType->name = name;
    return typeCheckArguments(arguments, ctorType, env, loc())->returnType;
  }
  throw TypeError(loc(), "Undefined constructor: `%s`", name.c_str());
}

// function related types
Type *FunctionType::typeof(EnvPtr env) {
  auto t = new TypeFunction();
  t->generics = generics;

  for (auto &g : t->generics) {
    g = uniqueName(g, env);
  }

  loadGenerics(t->generics, env);

  for (auto p : params) {
    auto tt = p->typeof(env);
    t->types.push_back(tt);

    if (!t->usesInterface && tt != s_interface) {
      t->usesInterface = usesInterface(tt, env);
    }
  }

  t->interface = s_interface;
  t->returnType = returnType->typeof(env);
  return t;
}

Type *Prototype::typeof(EnvPtr env) {
  auto t = dynamic_cast<TypeFunction *>(FunctionType::typeof(env));
  name += s_implementationName;
  t->name = name;
  env->get(name).type = t;
  return t;
}

Type *Call::typeof(EnvPtr env) {
  env = env->create();
  auto calleeType = callee->typeof(env);
  auto fnType = dynamic_cast<TypeFunction *>(calleeType);
  if (!fnType) {
    throw TypeError(loc(), "Can't find type information for function call");
  }

  if (fnType->interface && !env->get(fnType->interface->genericTypeName).type) {
    env->create(fnType->interface->genericTypeName).type = fnType->interface;
  }

  auto t = typeCheckArguments(arguments, fnType, env, loc());

  if (fnType->interface) {
    auto ident = asIdentifier(this->callee);
    auto t = env->get(fnType->interface->genericTypeName).type;
    if (!t) {
      throw TypeError(loc(), "Cannot resolve which interface implementation should be used");
    }
    auto name = ident->name + "$" + t->toString();
    if (env->get(name).type) {
      ident->name = name;
    }
  }

  if (fnType->usesInterface) {
    // TODO: support anonymous lambdas
    if (auto ident = asIdentifier(callee)) {
      std::string name = "";
      auto original = dynamic_cast<Function *>(env->get(ident->name).node);
      assert(original);

      auto newType = new TypeFunction(*t);
      newType->returnType = fnType->returnType;
      newType->usesInterface = false;

      for (const auto &tt : newType->types) {
        name += "$" + tt->toString();
      }

      ident->name += name;

      if (!original->instances[name]) {
        // clone AST and re-run the naming phase
        auto fn = asFunction(original->copy());
        fn->naming(env);

        // cache and append types used to instantiate to the name
        original->instances[name] = fn;
        fn->name += name;

        // export the original function in the new context
        fn->body->env->create(original->name).type = fnType;
        fn->body->env->create(original->name).node = original;

        // explicitly export the specialised type and erase the original one
        fn->type = nullptr;
        fn->body->env->create(fn->name).type = newType;

        // finally recompute the types for the whole specialised function body
        fn->typeof(fn->body->env);
      }
    }
  }

  return t->returnType;
}

Type *Function::typeof(EnvPtr env) {
  Type *t;
  if (type) {
    name += s_implementationName;
    t = type->typeof(body->env);
    env->get(name).type = t;
  } else {
    t = body->env->get(name).type;
    if (!s_implementationName.empty()) {
      name += s_implementationName;
      env->get(name).type = t;
    }
  }

  auto fnType = dynamic_cast<TypeFunction *>(t);

  // TODO: proper error message
  assert(fnType);
  assert(fnType->types.size() == parameters.size());

  if (fnType->usesInterface) {
    return fnType;
  }

  // isolate function params
  for (unsigned i = 0; i < parameters.size(); i++) {
    auto t = fnType->types[i];
    body->env->create(parameters[i]->name).type = t;
  }

  auto bodyType = body->typeof(body->env);
  if (!typeEq(fnType->returnType, bodyType, body->env)) {
    throw TypeError(body->loc(), "Invalid return type for function: expected `%s` but got `%s`", fnType->returnType->toString().c_str(), bodyType->toString().c_str());
  }

  return t;
}

}
}
