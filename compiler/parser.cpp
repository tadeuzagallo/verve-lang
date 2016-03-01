#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {

  std::shared_ptr<AST::Program> Parser::parse(void) {
    m_ast = std::make_shared<AST::Program>();

    while (m_lexer.nextToken()->type != Token::Type::END) {
      parseFunction();
    }

    return m_ast;
  }

  void Parser::parseFunction(void) {
    m_lexer.ensure(Token::Type::L_PAREN);

    auto callee = std::static_pointer_cast<Token::ID>(m_lexer.token(Token::Type::ID));
    m_ast->addNode(std::make_shared<AST::Call>(callee->name));

    m_lexer.ensure(Token::Type::R_PAREN);
  }

}
