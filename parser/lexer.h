#include <fstream>
#include <memory>

#include "./token.h"

#pragma once

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

      void rewind();

      void _Noreturn invalidType();
      void printSource();
      void printSource(Loc loc);

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
