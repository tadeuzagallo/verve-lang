#include "type_checker.h"

namespace ceos {

static Type *getType(AST::NodePtr node, Environment *env, Lexer &lexer) {
  switch (node->type) {
    case AST::Type::String:
      return env->get("String");

    case AST::Type::Number:
      return env->get("Int");

    case AST::Type::Function:
      return env->get(AST::asFunction(node)->name);

    case AST::Type::Identifier:
      return env->get(AST::asIdentifier(node)->name);

    case AST::Type::FunctionParameter:
      return env->get(AST::asFunctionParameter(node)->name);

    case AST::Type::Block:
      {
        auto block = AST::asBlock(node);
        return block->nodes.empty() ? env->get("Void") : getType(block->nodes.back(), env, lexer);
      }

    case AST::Type::Call:
      {
        auto call = AST::asCall(node);
        auto type = getType(call->callee, env, lexer);
        if (auto fnType = dynamic_cast<TypeFunction *>(type)) {
          return fnType->returnType;
        } else if (auto ctorType = dynamic_cast<TypeConstructor *>(type)) {
          return ctorType->type->returnType;
        } else {
          throw std::runtime_error("type error");
        }
      }

    case AST::Type::If:
      {
        auto iff = AST::asIf(node);
        auto iffType = getType(iff->ifBody, env, lexer);
        if (iff->elseBody) {
          auto elseType = getType(iff->elseBody, env, lexer);
          // return the least specific type
          if (iffType->accepts(elseType)) {
            return iffType;
          } else if (elseType->accepts(iffType)) {
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

    default:
      throw std::runtime_error("unhandled node");
  }
}

void TypeChecker::checkCall(AST::CallPtr call, Environment *env, Lexer &lexer) {
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

  TypeImplementation *implementation = nullptr;
  TypeMap generics;

  for (unsigned i = 0; i < fnType->types.size(); i++) {
    auto arg = call->arguments[i];

    auto expected = fnType->types[i];
    auto actual = getType(arg, env, lexer);

    GenericType *gt;

    Type *t;
    while ((gt = dynamic_cast<GenericType *>(actual)) && (t = env->get(gt->typeName))) {
      actual = t;
    }

    if (
        fnType->isVirtual &&
        (gt = dynamic_cast<GenericType *>(expected)) &&
        gt->typeName == fnType->interface->genericTypeName
       )
    {
      if (implementation) {
        goto skip;
      }

      for (auto impl : fnType->interface->implementations) {
        if (impl->type->accepts(actual)) {
          implementation = impl;
          AST::asIdentifier(callee)->name += implementation->type->toString();
          goto skip;
        }
      }
    }

    if ((gt = dynamic_cast<GenericType *>(expected))) {
      if (generics[gt->typeName]) {
        expected = generics[gt->typeName];
      } else {
        auto it = std::find(fnType->generics.begin(), fnType->generics.end(), gt->typeName);
        if (it != fnType->generics.end()) {
          generics[gt->typeName] = actual;
          goto skip;
        }
      }
    }

    if (!expected->accepts(actual)) {
      fprintf(stderr, "Expected `%s` but got `%s` on arg #%d for function `%s`\n",
          expected->toString().c_str(),
          actual->toString().c_str(),
          i + 1,
          fnType->name.c_str());
      lexer.printSource(arg->loc);
      throw std::runtime_error("type error");
    }

skip:
    continue;
  }

  GenericType *gt;
  if ((gt = dynamic_cast<GenericType *>(fnType->returnType))) {
    fnType->returnType = generics[gt->typeName];
  }
}

void TypeChecker::checkReturnType(Type *expected, AST::BlockPtr body, Environment *env, Lexer &lexer) {
  auto actual = getType(body, env, lexer);
  if (!expected->accepts(actual)) {
    fprintf(stderr, "Type Error: Invalid return type for function: expected `%s` but got `%s`\n", expected->toString().c_str(), actual->toString().c_str());
    lexer.printSource(body->loc);
    throw std::runtime_error("type error");
  }
}

}
