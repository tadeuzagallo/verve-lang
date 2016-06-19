#include "type_checker.h"

namespace ceos {

static Type *getType(AST::NodePtr node, Environment *env, Lexer &lexer);

static Type *typeOfCall(AST::CallPtr call, Environment *env, Lexer &lexer) {
  auto callee = call->callee;
  auto calleeType = getType(callee, env, lexer);
  TypeFunction *fnType;

  if (auto ctorType = dynamic_cast<TypeConstructor *>(calleeType)) {
    call->isConstructor = true;
    call->tag = ctorType->tag;
    call->size = ctorType->size;
    fnType = ctorType->type;
  } else if(!(fnType = dynamic_cast<TypeFunction *>(calleeType))) {
    if (lexer.next('{')) {
      fprintf(stderr, "Parse error: Maybe type information is missing for function declaration?\n");
    } else {
      fprintf(stderr, "Can't find type information for function call\n");
    }

    lexer.printSource(call->loc);
    throw std::runtime_error("type error");
  }

  if (call->arguments.size() != fnType->types.size()) {
    fprintf(stderr, "Wrong number of arguments for function call\n");
    lexer.printSource(call->loc);
    throw std::runtime_error("type error");
  }

  if (fnType->isVirtual) {
    env->types[fnType->interface->genericTypeName] = fnType->interface;
  }

  for (auto generic : fnType->generics) {
    env->types[generic] = new GenericType(generic);
  }

  for (unsigned i = 0; i < fnType->types.size(); i++) {
    auto arg = call->arguments[i];

    auto expected = fnType->types[i];
    auto actual = getType(arg, env, lexer);

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

  if (auto gt = dynamic_cast<GenericType *>(fnType->returnType)) {
    return env->get(gt->typeName);
  }

  return fnType->returnType;
}

static Type *getType(AST::NodePtr node, Environment *env, Lexer &lexer) {
  switch (node->type) {
    case AST::Type::String:
      return env->get("String");

    case AST::Type::BinaryOperation:
    case AST::Type::UnaryOperation:
    case AST::Type::Number:
      return env->get("Int");

    case AST::Type::Function:
      return env->get(AST::asFunction(node)->name);

    case AST::Type::Identifier:
      return env->get(AST::asIdentifier(node)->name);

    case AST::Type::FunctionParameter:
      return env->get(AST::asFunctionParameter(node)->name);

    case AST::Type::StackLoad:
      return getType(AST::asStackLoad(node)->value, env, lexer);

    case AST::Type::Block:
      {
        auto block = AST::asBlock(node);
        return block->nodes.empty() ? env->get("Void") : getType(block->nodes.back(), block->env.get() ?: env, lexer);
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

    case AST::Type::ObjectLoad:
      {
        auto load = AST::asObjectLoad(node);
        auto type = env->get(load->constructorName);
        if (auto ctor = dynamic_cast<TypeConstructor *>(type)) {
          return ctor->type->types[load->offset];
        }
        throw std::runtime_error("type error");
      }

    case AST::Type::List:
      {
        auto dataType = dynamic_cast<DataType *>(env->get("List"));
        Type *t = nullptr;
        for (auto i : AST::asList(node)->items) {
          auto type = getType(i, env, lexer);

          if (!t) {
            t = type;
          } else {
            assert(t == type);
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
          auto type = getType(kase, env, lexer);
          if (!t) {
            t = type;
          } else {
            assert(type == t);
          }
        }
        return t;
      }

    case AST::Type::Case:
      {
        return getType(AST::asCase(node)->body, env, lexer);
      }

    case AST::Type::Let:
      return getType(AST::asLet(node)->block, env, lexer);

    default:
      throw std::runtime_error("unhandled node");
  }
}

void TypeChecker::checkCall(AST::CallPtr call, Environment *env, Lexer &lexer) {
  getType(call, env, lexer);
}

void TypeChecker::checkReturnType(Type *expected, AST::BlockPtr body, Environment *env, Lexer &lexer) {
  auto actual = getType(body, env, lexer);
  if (!expected->accepts(actual, env)) {
    fprintf(stderr, "Type Error: Invalid return type for function: expected `%s` but got `%s`\n", expected->toString().c_str(), actual->toString().c_str());
    lexer.printSource(body->loc);
    throw std::runtime_error("type error");
  }
}

void TypeChecker::checkPatternMatch(TypeConstructor *ctor, AST::NodePtr value, Loc loc, Environment *env, Lexer &lexer) {
  auto valueType = getType(value, env, lexer);
  if (!valueType->accepts(ctor, env)) {
    fprintf(stderr, "Type Error: Trying to pattern match value of type `%s` with constructor `%s`\n", valueType->toString().c_str(), ctor->toString().c_str());
    lexer.printSource(loc);
    throw std::runtime_error("type error");
  }
}

}
