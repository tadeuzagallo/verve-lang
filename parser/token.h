#include "utils/macros.h"

#include <string>
#include <cstdlib>

#pragma once

namespace ceos {
  struct Loc {
    unsigned start;
    unsigned end;
  };

  class Token {
    friend class Lexer;

    public:
      class ID;
      class Number;
      class String;

      ENUM(Type,
          END,
          ID,
          NUMBER,
          STRING,
          BASIC
        )

      Token(Type t) :
        type(t) {}

      Token(Type t, int num) :
        type(t)
      {
        value.number = num;
      }

      Token(Type t, const char *str) :
        type(t)
      {
        value.string = str;
      }

      Token(Token &&other) :
        type(other.type),
        loc(other.loc),
        value(other.value)
      {
        other.value.string = NULL;
      }


      Token &operator=(Token &) = delete;
      Token &operator=(Token &&other)
      {
        type = other.type;
        loc = other.loc;
        value = other.value;
        other.value.string = NULL;
        return *this;
      };

      ~Token() {
        if ((type == Token::STRING || type == Token::ID) && value.string != NULL) {
          free((void *)value.string);
          value.string = NULL;
        }
      }

      inline std::string string() {
        return std::string(value.string);
      }

      inline int number() {
        return value.number;
      }

      Type type;
      Loc loc;
    private:
      union {
        const char *string;
        int number;
      } value = {0};
  };
}
