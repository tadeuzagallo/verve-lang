#include "macros.h"

#ifndef CEOS_TOKEN_H
#define CEOS_TOKEN_H

namespace ceos {

  class Token {
    public:
      class ID;

      ENUM_CLASS(Type, END, L_PAREN, R_PAREN, ID)

      Token(Type t) : type(t) {}

      Type type;

  };

  class Token::ID : public Token {
    public:
    ID(std::string n) : Token(Token::Type::ID), name(n) {}

    std::string name;
  };
}

#endif
