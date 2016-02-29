#include "lexer.h"

#include "token.h"

#include <cassert>
#include <sstream>

namespace ceos {

  std::shared_ptr<Token> Lexer::nextToken(void) {
    char c ;

    do {
      c = m_input.get();
    } while(isspace(c));

    switch (c) {
      case '(':
        m_token = std::make_shared<Token>(Token::L_PAREN);
        break;

      case ')':
        m_token = std::make_shared<Token>(Token::R_PAREN);
        break;

      case EOF:
        m_token = std::make_shared<Token>(Token::END);
        break;

      default:
        if (isalpha(c)) {
          std::stringstream id;
          do {
            id << c;
          } while (isalpha(c = m_input.get()));

          m_input.unget();

          m_token = std::make_shared<class Token::ID>(id.str());
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

  void Lexer::ensure(Token::Type type) {
    assert(m_token->type == type);
    nextToken();
  }
}
