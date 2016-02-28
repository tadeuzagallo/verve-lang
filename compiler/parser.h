#ifndef CEOS_PARSER_H
#define CEOS_PARSER_H

namespace ceos {

  class Lexer;
  class AST;

  class Parser {
    public:
      Parser(const Lexer &lexer) : m_lexer(lexer) {}

      const AST &parse(void) const;

    private:
      const Lexer &m_lexer;
  };

}

#endif
