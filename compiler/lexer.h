#include <fstream>
#include <memory>

#include "./token.h"

#ifndef CEOS_LEXER_H
#define CEOS_LEXER_H

namespace ceos {

  class Lexer {
    public:
      Lexer(std::ifstream &input) : m_input(input), m_token(nullptr) {}

      std::shared_ptr<Token> nextToken(void);

      std::shared_ptr<Token> token(Token::Type);

      std::shared_ptr<Token> token(void);

      void ensure(Token::Type);

      static void assertType(Token::Type, Token::Type);
      static void invalidType(Token::Type);

    private:
      std::ifstream &m_input;
      std::shared_ptr<Token> m_token;
  };

}

#endif
