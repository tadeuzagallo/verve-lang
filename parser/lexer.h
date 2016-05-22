#include <fstream>
#include <memory>

#include "./token.h"

#pragma once

namespace ceos {
  struct Pos {
    int line;
    int column;
  };

  class Lexer {
    public:
      Lexer(const char *input, size_t offset) :
        m_input(input),
        m_pos(0),
        m_offset(offset),
        m_token(Token(Token::END)),
        m_prevToken(Token(Token::END))
      {
        nextToken();
      }

      Token &token();
      Token &token(Token::Type);

      void rewind();
      void rewind(Loc &loc);

      bool next(char c);
      bool skip(char c);
      void match(char c);

      void _Noreturn invalidType();
      void printSource();
      void printSource(Loc loc);
      void _Noreturn error(Loc loc, const char *, ...);

    private:
      void nextToken();
      char nextChar();

      Pos getSourcePosition(Loc loc);

      const char *m_input;
      size_t m_pos;
      size_t m_offset;
      Token m_token;
      Token m_prevToken;
  };

}
