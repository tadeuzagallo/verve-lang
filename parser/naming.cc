#include "ast.h"

namespace Verve {
namespace AST {

void Program::naming(EnvPtr env) {
  body->naming(env);
}

void Block::naming(EnvPtr env) {
  this->env = env;
  for (const auto &node : nodes) {
    node->naming(env);
  }
}

void Identifier::naming(EnvPtr env) {
  const auto &node = env->get(name).node;
  bool shouldCapture = false;
  if (auto ident = dynamic_cast<Identifier *>(node)) {
    auto nodesEnv = env->envFor(ident->name);
    if (nodesEnv != env) {
      auto s = env;
      while (s != nodesEnv) {
        if (s->escapes) {
          shouldCapture = true;
          break;
        }
        s = s->parent();
      }

      if (shouldCapture) {
        ident->isCaptured = true;
        nodesEnv->isRequired = true;
        env->capturesScope = true;
      }
    }
  }
  if (!shouldCapture) {
    if (auto param = dynamic_cast<FunctionParameter *>(node)) {
      isFunctionParameter = true;
      index = param->index;
    }
  }
}

void FunctionParameter::naming(EnvPtr env) {
  env->create(name).node = this;
}

void Call::naming(EnvPtr env) {
  callee->naming(env);
  for (const auto &arg : arguments) {
    arg->naming(env);
  }
}

void If::naming(EnvPtr env) {
  condition->naming(env);

  env = env->create();
  ifBody->naming(env);

  if (elseBody) {
    elseBody->naming(env);
  }
}

void BinaryOperation::naming(EnvPtr env) {
  lhs->naming(env);
  rhs->naming(env);
}

void UnaryOperation::naming(EnvPtr env) {
  operand->naming(env);
}

void List::naming(EnvPtr env) {
  for (const auto &item : items) {
    item->naming(env);
  }
}

void Pattern::naming(EnvPtr env) {
  for (const auto &ident : values) {
    env->create(ident->name).node = ident.get();
  }
}

void Case::naming(EnvPtr env) {
  env = env->create();
  pattern->naming(env);
  body->naming(env);
}

void Match::naming(EnvPtr env) {
  value->naming(env);
  for (const auto &kase : cases) {
    kase->naming(env);
  }
}

void Let::naming(EnvPtr env) {
  env = env->create();
  for (const auto &assignment : assignments) {
    assignment->naming(env);
  }
  block->naming(env);
}

void Assignment::naming(EnvPtr env) {
  switch (kind) {
    case Assignment::Identifier:
      env->create(left.ident->name).node = value.get();
      break;
    case Assignment::Pattern:
      left.pattern->value = value;
      left.pattern->naming(env);
      break;
    default:
      throw std::runtime_error("Unhandled assignment type");
  }
  value->naming(env);
}

void Constructor::naming(EnvPtr env) {
  for (const auto &arg : arguments) {
    arg->naming(env);
  }
}

void Interface::naming(EnvPtr env) {
  env->create(name).node = this;
  this->env = env->create();
  for (const auto &fn : functions) {
    env->create(fn->getName()).node = fn.get();
    fn->naming(this->env);
  }
}

void Implementation::naming(EnvPtr env) {
  this->env = env->create();
  for (const auto &fn : functions) {
    fn->naming(this->env);
  }
}

void Prototype::naming(EnvPtr env) {
  env->create(name).node = this;
}

void Function::naming(EnvPtr env) {
  if (type) {
    type->naming(env);
  }
  env = env->create();
  env->escapes = true;
  for (const auto &param : parameters) {
    param->naming(env);
  }
  body->naming(env);
}

}
}
