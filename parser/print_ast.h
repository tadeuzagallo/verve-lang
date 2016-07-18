#include "ast.h"

#include <string>

namespace Verve {
  struct ASTPrinter {
    static void dump(AST::ProgramPtr ast);
    void print(unsigned depth, const char *format, ...);

    unsigned indentation = 2;
    bool inlineNext = false;
  private:
    AST::ProgramPtr m_ast;
  };
}
