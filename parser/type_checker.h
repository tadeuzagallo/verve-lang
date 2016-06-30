#include "ast.h"
#include "environment.h"
#include "lexer.h"
#include "type.h"

namespace verve {
  class TypeChecker {
    public:
      static Type *typeof(AST::NodePtr call, EnvPtr env, Lexer &lexer);
  };
}
