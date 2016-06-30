#include "utils/macros.h"

#include <cstdlib>
#include <string>
#include <unordered_map>

#pragma once

#define TUPLE_TOKEN(__char1, __char2) \
  ((__char2 << 8) | __char1)

namespace Verve {
  struct Loc {
    unsigned start;
    unsigned end;
  };

  class Token {
    friend class Lexer;

    public:
      ENUM(Type,
          END,
          LCID,
          UCID,
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
        if ((type == Token::STRING || type == Token::LCID || type == Token::UCID) && value.string != NULL) {
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

      static auto &precedenceTable() {
        if (s_precedenceTable.empty()) {
          s_precedenceTable = {
            // 0
            { TUPLE_TOKEN('&', '&'), 0 },
            { TUPLE_TOKEN('|', '|'), 0 },

            // 1
            { TUPLE_TOKEN('=', '='), 1 },
            { TUPLE_TOKEN('!', '='), 1 },

            // 2
            { '<', 2 },
            { '>', 2 },
            { TUPLE_TOKEN('<', '='), 2 },
            { TUPLE_TOKEN('>', '='), 2 },

            // 3
            { '+', 3 },
            { '-', 3 },

            // 4
            { '*', 4 },
            { '/', 4 },
            { '%', 4 },

            // 5
            { '!', 5 },
            { '-', 5 },
          };
        }
        return s_precedenceTable;
      }

      static int precedence(Token &token) {
        auto &table = precedenceTable();
        auto it = table.find(token.number());

        if (it != table.end()) {
          return it->second;
        }

        return -1;
      }

      static bool isUnaryOperator(Token &token) {
        return token.type == Token::BASIC && (
            token.number() == '!' ||
            token.number() == '-');
      }

      Type type;
      Loc loc;
    private:
      union {
        const char *string;
        int number;
      } value = {0};

      static std::unordered_map<int, int> s_precedenceTable;
  };
}
