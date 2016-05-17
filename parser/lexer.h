#include <fstream>
#include <memory>

#include "./token.h"

#ifndef CEOS_LEXER_H
#define CEOS_LEXER_H

namespace ceos {

  class Lexer {
    public:
      Lexer(const char *input, size_t offset) :
        m_input(input),
        m_pos(0),
        m_offset(offset),
        m_token(nullptr),
        m_prevToken(nullptr)
      {
        nextToken();
      }

      Token *token(Token::Type);
      Token *token();

      bool skip(Token::Type);

      void ensure(Token::Type);

      void _Noreturn invalidType();
      void printSource();

    private:
      void nextToken();
      char nextChar();

      const char *m_input;
      size_t m_pos;
      size_t m_offset;
      Token *m_token;
      Token *m_prevToken;
  };

}

#endif
