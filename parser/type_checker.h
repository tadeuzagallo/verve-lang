#include "ast.h"
#include "environment.h"
#include "lexer.h"
#include "type.h"

namespace ceos {
  class TypeChecker {
    public:
      static void check(AST::NodePtr call, EnvPtr env, Lexer &lexer);
  };
}
