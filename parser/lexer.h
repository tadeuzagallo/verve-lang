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

      bool next(int c);
      bool skip(int c);
      void match(int c);

      void _Noreturn invalidToken();
      void printSource();
      void printSource(Loc loc);
      void _Noreturn error(Loc loc, const char *, ...);

      static std::string tokenType(Token &token);

    private:
      void nextToken();
      char nextChar();

      Pos getSourcePosition(Loc loc);

      static std::string basicTokenToString(int t);

      const char *m_input;
      size_t m_pos;
      size_t m_offset;
      Token m_token;
      Token m_prevToken;
  };

}
