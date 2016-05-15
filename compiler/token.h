#include "macros.h"
#include <string>

#ifndef CEOS_TOKEN_H
#define CEOS_TOKEN_H

namespace ceos {
  struct Loc {
    unsigned start;
    unsigned end;
  };

  class Token {
    public:
      class ID;
      class Number;
      class String;

      ENUM_CLASS(Type,
          END,
          L_PAREN,
          R_PAREN,
          ID,
          NUMBER,
          STRING,
          TYPE,
          ARROW,
          COMMA,
          L_BRACE,
          R_BRACE,
          L_ANGLE,
          R_ANGLE,
        )

      Token(Type t) : type(t) {}

      Type type;
      Loc loc;
  };

  class Token::ID : public Token {
    public:
      ID(std::string n) : Token(Type::ID), name(n) {}

      std::string name;
  };

  class Token::Number : public Token {
    public:
      Number(int v) : Token(Type::NUMBER), value(v) {}

      int value;
  };

  class Token::String : public Token {
    public:
      String(std::string str) : Token(Type::STRING), value(str) {}

      std::string value;
  };
}

#endif
