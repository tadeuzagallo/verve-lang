#include <memory>

#include "ast.h"

#ifndef CEOS_PARSER_H
#define CEOS_PARSER_H

namespace ceos {

  class Lexer;
  class AST;

  class Parser {
    public:
      Parser(Lexer &lexer) : m_lexer(lexer), m_ast(AST::Program()) {}

      AST::Program parse(void);

    private:
      void parseFunction(void);

      Lexer &m_lexer;
      AST::Program m_ast;
  };

}

#endif
