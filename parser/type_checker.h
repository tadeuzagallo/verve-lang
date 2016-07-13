#include "ast.h"
#include "environment.h"
#include "lexer.h"
#include "type.h"

namespace Verve {
  class TypeChecker {
    public:
      static void check(AST::ProgramPtr program, Lexer &lexer);
  };
}
