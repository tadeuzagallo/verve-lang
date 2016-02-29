#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {

  AST::Program Parser::parse(void) {
    while (m_lexer.nextToken()->type != Token::END) {
      parseFunction();
    }

    return m_ast;
  }

  void Parser::parseFunction(void) {
    m_lexer.ensure(Token::L_PAREN);

    auto callee = std::static_pointer_cast<class Token::ID>(m_lexer.token(Token::ID));
    m_ast.addNode(std::make_shared<AST::Call>(callee->name));

    m_lexer.ensure(Token::R_PAREN);
  }

}
