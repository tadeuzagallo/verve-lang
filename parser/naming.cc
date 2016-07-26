#include "naming.h"

#include "ast.h"

namespace Verve {
namespace AST {

namespace {
  struct PushEnv {
    PushEnv(Naming *naming) :
      m_naming(naming),
      m_oldEnv(naming->m_env)
    {
      m_naming->m_env = m_naming->m_env->create();
    };

    ~PushEnv() {
      m_naming->m_env = m_oldEnv;
    }

    Naming *m_naming;
    EnvPtr m_oldEnv;
  };
}

void Naming::visitBlock(Block *block) {
  block->env = m_env;
  Visitor::visitBlock(block);
}

void Naming::visitIdentifier(Identifier *ident) {
  auto *node = m_env->get(ident->name).node;
  bool shouldCapture = false;
  if (auto ident = dynamic_cast<Identifier *>(node)) {
    auto nodesEnv = m_env->envFor(ident->name);
    if (nodesEnv != m_env) {
      auto s = m_env;
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
        m_env->capturesScope = true;
      }
    }
  }
  if (!shouldCapture) {
    if (auto param = dynamic_cast<FunctionParameter *>(node)) {
      ident->isFunctionParameter = true;
      ident->index = param->index;
    }
  }
}

void Naming::visitFunctionParameter(FunctionParameter *param) {
  m_env->create(param->name).node = param;
}

void Naming::visitIf(If *iff) {
  iff->condition->visit(this);

  PushEnv env(this);
  iff->ifBody->visit(this);

  if (iff->elseBody) {
    iff->elseBody->visit(this);
  }
}

void Naming::visitPattern(Pattern *pattern) {
  for (const auto &ident : pattern->values) {
    m_env->create(ident->name).node = ident.get();
  }
}

void Naming::visitCase(Case *kase) {
  PushEnv env{this};
  Visitor::visitCase(kase);
}

void Naming::visitLet(Let *let) {
  PushEnv env{this};
  Visitor::visitLet(let);
}

void Naming::visitAssignment(Assignment *assignment) {
  if (assignment->kind == Assignment::Identifier) {
    m_env->create(assignment->left.ident->name).node = assignment->value.get();
  } else if (assignment->kind == Assignment::Pattern) {
    assignment->left.pattern->value = assignment->value;
    assignment->left.pattern->visit(this);
  } else {
    throw std::runtime_error("Unhandled assignment type");
  }
  assignment->value->visit(this);
}

void Naming::visitInterface(Interface *interface) {
  m_env->create(interface->name).node = interface;

  PushEnv env{this};
  interface->env = m_env;

  for (const auto &fn : interface->functions) {
    env.m_oldEnv->create(fn->getName()).node = fn.get();
    fn->visit(this);
  }
}

void Naming::visitImplementation(Implementation *impl) {
  PushEnv env{this};
  impl->env = m_env;
  for (const auto &fn : impl->functions) {
    env.m_oldEnv->create(fn->getName()).node = fn.get();
    fn->visit(this);
  }
}

void Naming::visitPrototype(Prototype *prototype) {
  m_env->create(prototype->name).node = prototype;
}

void Naming::visitFunction(Function *fn) {
  if (fn->type) {
    m_env->create(fn->name).node = fn;
  }
  PushEnv env{this};
  m_env->escapes = true;
  for (const auto &param : fn->parameters) {
    param->visit(this);
  }
  fn->body->visit(this);
}

}
}
