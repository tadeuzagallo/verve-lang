#ifndef CEOS_TOKEN_H
#define CEOS_TOKEN_H

namespace ceos {

  class Token {
    public:
      class ID;

      enum class Type {
        END = 1,
        L_PAREN,
        R_PAREN,
        ID,
      };


      Token(Type t) : type(t) {}
      Type type;

      static const char *typeName(Type t) {
        return (const char *[]) {
          "",
          "END",
          "L_PAREN",
          "R_PAREN",
          "ID",
        }[(int)t];
      }
  };

  class Token::ID : public Token {
    public:
    ID(std::string n) : Token(Token::Type::ID), name(n) {}

    std::string name;
  };
}

#endif
