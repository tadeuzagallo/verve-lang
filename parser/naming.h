#include "ast_visitor.h"

#include "environment.h"

namespace Verve {
namespace AST {
  namespace {
    struct PushEnv;
  }

  class Naming : public Visitor {
    friend PushEnv;

  public:
    Naming(EnvPtr env) :
      m_env(env) {}

  private:
    virtual void visitBlock(Block *);
    virtual void visitIdentifier(Identifier *);
    virtual void visitFunctionParameter(FunctionParameter *);
    virtual void visitIf(If *);
    virtual void visitPattern(Pattern *);
    virtual void visitCase(Case *);
    virtual void visitLet(Let *);
    virtual void visitAssignment(Assignment *);
    virtual void visitInterface(Interface *);
    virtual void visitImplementation(Implementation *);
    virtual void visitPrototype(Prototype *);
    virtual void visitFunction(Function *);

    EnvPtr m_env;
  };
}
}
