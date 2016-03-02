#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {

  std::shared_ptr<AST::Program> Parser::parse(void) {
    m_ast = std::make_shared<AST::Program>();

    while (m_lexer.nextToken()->type != Token::Type::END) {
      std::shared_ptr<AST> node = parseFactor();
      m_ast->addNode(node);
    }

    return m_ast;
  }

  std::shared_ptr<AST> Parser::parseFactor() {
    switch (m_lexer.token()->type) {
      case Token::Type::L_PAREN:
        return parseCall();
      case Token::Type::NUMBER:
        return parseNumber();
      default:
        Lexer::assertType(m_lexer.token()->type, Token::Type::END);
        throw "Unexpected token";
    }
  }

  std::shared_ptr<AST::Number> Parser::parseNumber() {
    auto number = std::static_pointer_cast<Token::Number>(m_lexer.token(Token::Type::NUMBER));
    return std::make_shared<AST::Number>(number->value);
  }

  std::shared_ptr<AST::Call> Parser::parseCall(void) {
    m_lexer.ensure(Token::Type::L_PAREN);

    auto callee = std::static_pointer_cast<Token::ID>(m_lexer.token(Token::Type::ID));

    auto call = std::make_shared<AST::Call>(callee->name);

    while (m_lexer.token()->type != Token::Type::R_PAREN) {
      call->params.push_back(parseFactor());
    }

    m_lexer.ensure(Token::Type::R_PAREN);

    return call;
  }

}
