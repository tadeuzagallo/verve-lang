#include <memory>

#include "ast.h"

#ifndef CEOS_PARSER_H
#define CEOS_PARSER_H

namespace ceos {

  class Lexer;
  class AST;

  class Parser {
    public:
      Parser(Lexer &lexer) : m_lexer(lexer) {}

      std::shared_ptr<AST::Program> parse(void);

    private:
      std::shared_ptr<AST::Call> parseCall(void);
      std::shared_ptr<AST::Number> parseNumber(void);
      std::shared_ptr<AST::ID> parseID(void);
      std::shared_ptr<AST> parseFactor(void);

      Lexer &m_lexer;
      std::shared_ptr<AST::Program> m_ast;
  };

}

#endif
