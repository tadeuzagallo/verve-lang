#include "lexer.h"

#include "token.h"

#include <cassert>
#include <iostream>
#include <sstream>

namespace ceos {

  std::shared_ptr<Token> Lexer::nextToken(void) {
    char c ;

    do {
      c = m_input.get();
    } while(isspace(c));

    switch (c) {
      case '(':
        m_token = std::make_shared<Token>(Token::Type::L_PAREN);
        break;

      case ')':
        m_token = std::make_shared<Token>(Token::Type::R_PAREN);
        break;

      case EOF:
        m_token = std::make_shared<Token>(Token::Type::END);
        break;

      default:
        if (isnumber(c)) {
          int number = 0;
          do {
            number *= 10;
            number += c - '0';
          } while (isnumber(c = m_input.get()));

          m_input.unget();

          m_token = std::make_shared<Token::Number>(number);
        } else if (isalpha(c)) {
          std::stringstream id;
          do {
            id << c;
          } while (isalpha(c = m_input.get()));

          m_input.unget();

          m_token = std::make_shared<Token::ID>(id.str());
        }
    }

    return m_token;
  }

  std::shared_ptr<Token> Lexer::token(Token::Type type) {
    auto token = m_token;
    ensure(type);
    return token;
  }

  std::shared_ptr<Token> Lexer::token(void) {
    return m_token;
  }

  void Lexer::ensure(Token::Type type, bool skip) {
    assertType(m_token->type, type);

    if (skip) {
      nextToken();
    }
  }

  void Lexer::ensure(Token::Type type) {
    ensure(type, true);
  }

  void Lexer::assertType(Token::Type a, Token::Type b) {
    if (a != b) {
      std::cerr << "Invalid type found: expected `" << Token::typeName(a) << "` to be `" << Token::typeName(b) << "`" << "\n";
      throw "Parser error";
    }
  }
}
