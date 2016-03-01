#include "macros.h"

#ifndef CEOS_TOKEN_H
#define CEOS_TOKEN_H

namespace ceos {

  class Token {
    public:
      class ID;
      class Number;

      ENUM_CLASS(Type, END, L_PAREN, R_PAREN, ID, NUMBER)

      Token(Type t) : type(t) {}

      Type type;

  };

  class Token::ID : public Token {
    public:
      ID(std::string n) : Token(Type::ID), name(n) {}

      std::string name;
  };

  class Token::Number : public Token {
    public:
      Number(int v) : Token(Type::NUMBER), value(v) {};

      int value;
  };
}

#endif
