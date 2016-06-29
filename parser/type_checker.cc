#include "type_checker.h"

namespace ceos {

static Type *getType(AST::NodePtr node, EnvPtr env, Lexer &lexer);

static Type *simplifyType(Type *type, EnvPtr env) {
  if (auto gt = dynamic_cast<GenericType *>(type)) {
    auto t = env->get(gt->typeName);
    if (t != type) {
      return simplifyType(t, env);
    }
  } else if (auto dti = dynamic_cast<DataTypeInstance *>(type)) {
    dti->dataType = simplifyType(dti->dataType, env);
    for (unsigned i = 0; i < dti->types.size(); i++) {
      dti->types[i] = simplifyType(dti->types[i], env);
    }
  } else if (auto interface = dynamic_cast<TypeInterface *>(type)) {
    auto t = env->get(interface->genericTypeName);
    if (t && t != type && !dynamic_cast<GenericType *>(t)) {
      return simplifyType(t, env);
    }
  }
  return type;
}

static void loadFnGenerics(TypeFunction *fnType, EnvPtr env) {
  if (fnType->isVirtual) {
    env->types[fnType->interface->genericTypeName] = fnType->interface;
  }

  for (auto generic : fnType->generics) {
    env->types[generic] = new GenericType(generic);
  }
}

static Type *typeCheckArguments(std::vector<AST::NodePtr> &arguments, TypeFunction *fnType, EnvPtr env, Lexer &lexer, Loc &loc) {
  if (arguments.size() != fnType->types.size()) {
    fprintf(stderr, "Wrong number of arguments for function call\n");
    lexer.printSource(loc);
    throw std::runtime_error("type error");
  }

  loadFnGenerics(fnType, env);

  for (unsigned i = 0; i < fnType->types.size(); i++) {
    auto arg = arguments[i];

    auto expected = simplifyType(fnType->types[i], env);
    auto actual = simplifyType(getType(arg, env, lexer), env);

    if (!expected->accepts(actual, env)) {
      fprintf(stderr, "Expected `%s` but got `%s` on arg #%d for function `%s`\n",
          expected->toString().c_str(),
          actual->toString().c_str(),
          i + 1,
          fnType->name.c_str());
      lexer.printSource(arg->loc);
      throw std::runtime_error("type error");
    }
  }

  if (auto et = dynamic_cast<EnumType *>(fnType->returnType)) {
    if (et->generics.size()) {
      auto returnType = new DataTypeInstance();
      returnType->dataType = et;
      for (auto t : et->generics) {
        returnType->types.push_back(env->get(t));
      }
      return returnType;
    }
  }

  return simplifyType(fnType->returnType, env);
}

static Type *typeOfCall(AST::CallPtr call, EnvPtr env, Lexer &lexer) {
  auto callee = call->callee;
  auto calleeType = getType(callee, env, lexer);
  TypeFunction *fnType;

  if(!(fnType = dynamic_cast<TypeFunction *>(calleeType))) {
    if (lexer.next('{')) {
      fprintf(stderr, "Parse error: Maybe type information is missing for function declaration?\n");
    } else {
      fprintf(stderr, "Can't find type information for function call\n");
    }

    lexer.printSource(call->loc);
    throw std::runtime_error("type error");
  }

  auto returnType = typeCheckArguments(call->arguments, fnType, env, lexer, call->loc);

  if (fnType->isVirtual) {
    auto name = AST::asIdentifier(callee)->name + env->types[fnType->interface->genericTypeName]->toString();
    if (env->get(name)) {
      AST::asIdentifier(callee)->name = name;
    }
  }

  return simplifyType(returnType, env);
}

static Type *typeCheckFunction(AST::FunctionPtr fn, EnvPtr env, Lexer &lexer) {
  auto type = env->get(fn->name);
  auto fnType = dynamic_cast<TypeFunction *>(type);
  assert(fnType);

  loadFnGenerics(fnType, env);

  auto expected = simplifyType(fnType->returnType, env);
  auto actual = simplifyType(getType(fn->body, env, lexer), env);

  if (!expected->accepts(actual, env)) {
    fprintf(stderr, "Type Error: Invalid return type for function: expected `%s` but got `%s`\n", expected->toString().c_str(), actual->toString().c_str());
    lexer.printSource(fn->body->loc);
    throw std::runtime_error("type error");
  }

  auto t = new TypeFunction(*fnType);
  t->returnType = actual;

  return t;
}

static Type *_getType(AST::NodePtr node, EnvPtr env, Lexer &lexer) {
  switch (node->type) {
    case AST::Type::String:
      return env->get("string");

    case AST::Type::BinaryOperation:
    case AST::Type::UnaryOperation:
    case AST::Type::Number:
      return env->get("int");

    case AST::Type::Function:
      return typeCheckFunction(AST::asFunction(node), env, lexer);

    case AST::Type::Identifier:
      return env->get(AST::asIdentifier(node)->name);

    case AST::Type::FunctionParameter:
      return env->get(AST::asFunctionParameter(node)->name);

    case AST::Type::Block:
      {
        auto block = AST::asBlock(node);
        if (block->nodes.empty()) {
          return env->get("void");
        } else {
          Type *t;
          for (auto node : block->nodes) {
            t = getType(node, block->env ?: env, lexer);
          }
          return t;
        }
      }

    case AST::Type::Call:
      {
        auto call = AST::asCall(node);
        return typeOfCall(call, env, lexer);
      }

    case AST::Type::If:
      {
        auto iff = AST::asIf(node);
        auto iffType = getType(iff->ifBody, env, lexer);
        if (iff->elseBody) {
          auto elseType = getType(iff->elseBody, env, lexer);
          // return the least specific type
          if (iffType->accepts(elseType, env)) {
            return iffType;
          } else if (elseType->accepts(iffType, env)) {
            return elseType;
          } else {
            fprintf(stderr, "Type Error: `if` and `else` branches evaluate to different types\n");
            lexer.printSource(iff->loc);
            throw std::runtime_error("type error");
          }
        }
        return iffType;
      }

    case AST::Type::List:
      {
        auto dataType = dynamic_cast<EnumType *>(env->get("list"));
        Type *t = nullptr;
        for (auto i : AST::asList(node)->items) {
          auto type = simplifyType(getType(i, env, lexer), env);

          if (!t) {
            t = type;
          } else {
            assert(t->accepts(type, env));
          }
        }

        auto dti = new DataTypeInstance();
        dti->dataType = dataType;
        dti->types.push_back(t);
        return dti;
      }

    case AST::Type::Match:
      {
        auto match = AST::asMatch(node);
        assert(match->cases.size());
        Type *t = nullptr;
        for (auto kase : match->cases) {
          auto type = simplifyType(getType(kase, env, lexer), env);
          if (!t) {
            t = type;
          } else {
            assert(t->accepts(type, env) || type->accepts(t, env));
          }
        }
        return t;
      }

    case AST::Type::Case:
      {
        return getType(AST::asCase(node)->body, env, lexer);
      }

    case AST::Type::Let:
      {
        auto let = AST::asLet(node);
        for (auto assignment : let->assignments) {
          assert(getType(assignment, env, lexer));
        }
        return getType(let->block, env, lexer);
      }

    case AST::Type::Assignment:
      {
        auto assignment = AST::asAssignment(node);
        auto valueType = getType(assignment->value, env, lexer);
        if (assignment->left->type == AST::Type::Pattern) {
          auto pattern = AST::asPattern(assignment->left);
          auto patternType = env->get(pattern->constructorName);
          if (!valueType->accepts(patternType, env)) {
            fprintf(stderr, "Type Error: Trying to pattern match value of type `%s` with constructor `%s`\n", valueType->toString().c_str(), patternType->toString().c_str());
            lexer.printSource(pattern->loc);
            throw std::runtime_error("type error");
          }
        } else {
          assert(assignment->left->type == AST::Type::Identifier);
        }
        return valueType;
      }

    case AST::Type::Constructor:
      {
        auto ctor = AST::asConstructor(node);
        auto type = env->get(ctor->name);
        auto ctorType = dynamic_cast<TypeConstructor *>(type);
        assert(ctorType);
        return typeCheckArguments(ctor->arguments, ctorType->type, env, lexer, ctor->loc);
      }

    default:
      throw std::runtime_error("unhandled node");
  }
}

static Type *getType(AST::NodePtr node, EnvPtr env, Lexer &lexer) {
  auto _env = std::make_shared<Environment>();
  _env->parent = env;

  auto t = _getType(node, _env, lexer);

  return t;
}

Type *TypeChecker::typeof(AST::NodePtr node, EnvPtr env, Lexer &lexer) {
  return getType(node, env, lexer);
}

void TypeChecker::check(AST::NodePtr node, EnvPtr env, Lexer &lexer) {
  if (!getType(node, env, lexer)) {
    fprintf(stderr, "Type Error\n");
    lexer.printSource(node->loc);
    throw std::runtime_error("type error");
  }
}

}
