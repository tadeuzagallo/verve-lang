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
          COMMA,
          L_BRACE,
          R_BRACE,
          L_ANGLE,
          R_ANGLE,
          COLON,
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
        if (value.string) {
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
      union {
        const char *string;
        int number;
      } value = {0};
  };
}
