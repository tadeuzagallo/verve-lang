#include "type_checker.h"

namespace ceos {

static Type *getType(AST::NodePtr node, Environment *env) {
  switch (node->type) {
    case AST::Type::String:
      return env->get("String");

    case AST::Type::Number:
      return env->get("Int");

    case AST::Type::Function:
      return getType(AST::asFunction(node)->name, env);

    case AST::Type::Identifier:
    case AST::Type::FunctionParameter:
      return env->get(AST::asIdentifier(node)->name);

    case AST::Type::Block:
      {
        auto block = AST::asBlock(node);
        return block->nodes.empty() ? env->get("Void") : getType(block->nodes.back(), env);
      }

    case AST::Type::Call:
      {
        auto call = AST::asCall(node);
        auto type = getType(call->callee, env);
        TypeFunction *fnType;
        if ((fnType = dynamic_cast<TypeFunction *>(type))) {
          return fnType->returnType;
        } else {
          throw std::runtime_error("type error");
        }
      }

    case AST::Type::If:
      {
        auto iff = AST::asIf(node);
        auto iffType = getType(iff->ifBody, env);
        if (iff->elseBody) {
          auto elseType = getType(iff->elseBody, env);
          assert(iffType->equals(elseType));
        }
        return iffType;
      }

    default:
      throw std::runtime_error("unhandled node");
  }
}

void TypeChecker::checkCall(AST::CallPtr call, Environment *env, Lexer &lexer) {
  auto callee = call->callee;
  auto calleeType = getType(callee, env);
  TypeFunction *fnType;

  if(!(fnType = dynamic_cast<TypeFunction *>(calleeType))) {
    fprintf(stderr, "Can't find type information for function call\n");
    lexer.printSource(call->loc);
    throw std::runtime_error("type error");
  }

  if (call->arguments.size() != fnType->types.size()) {
    fprintf(stderr, "Wrong number of arguments for function call\n");
    lexer.printSource(call->loc);
    throw std::runtime_error("type error");
  }

  for (unsigned i = 0; i < fnType->types.size(); i++) {
    auto arg = call->arguments[i];

    auto expected = fnType->types[i];
    auto actual = getType(arg, env);

    TypeInterface *ti;
    if ((ti = dynamic_cast<TypeInterface *>(expected))) {
      for (auto impl : ti->implementations) {
        if (impl->type->equals(actual)) {
          goto skip;
        }
      }
    }

    if (!expected->equals(actual)) {
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
}

}
