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
      std::shared_ptr<AST::TypeInfo> parseTypeInfo(std::shared_ptr<AST> &&id);
      std::shared_ptr<AST::Call> parseCall(std::shared_ptr<AST> &&callee);
      std::shared_ptr<AST::Number> parseNumber(void);
      std::shared_ptr<AST> parseID(void);
      std::shared_ptr<AST::String> parseString(void);
      std::shared_ptr<AST> parseFactor(void);
      std::shared_ptr<AST> parseIf(void);
      void parseBody(std::vector<std::shared_ptr<AST>> &body, Token::Type delim);

      Lexer &m_lexer;
      std::shared_ptr<AST::Program> m_ast;
  };

}

#endif
