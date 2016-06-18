#include "ast.h"
#include "environment.h"
#include "lexer.h"
#include "type.h"

namespace ceos {
  class TypeChecker {
    public:
      static void checkCall(AST::CallPtr call, Environment *env, Lexer &lexer);
      static void checkReturnType(Type *expected, AST::BlockPtr body, Environment *env, Lexer &lexer);
      static void checkPatternMatch(TypeConstructor *ctor, AST::NodePtr value, Loc loc, Environment *env, Lexer &lexer);

  };
}
