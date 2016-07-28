#include "ast/nodes.h"

#include "environment.h"
#include "lexer.h"
#include "type.h"

namespace Verve {
  class TypeChecker {
    public:
      static void check(AST::ProgramPtr program, EnvPtr env, Lexer &lexer);
  };
}
