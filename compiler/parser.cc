#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {

  std::shared_ptr<AST::Program> Parser::parse(void) {
    m_ast = std::make_shared<AST::Program>();

    m_lexer.nextToken();
    int start = -1;
    int end;
    while (m_lexer.token()->type != Token::Type::END) {
      std::shared_ptr<AST> node = parseFactor();
      m_ast->addNode(node);

      end = node->loc.end;
      if (start == -1) {
        start = end;
      }
    }

    m_ast->loc = { start, end };

    return m_ast;
  }

  std::shared_ptr<AST> Parser::parseFactor() {
    switch (m_lexer.token()->type) {
      case Token::Type::L_PAREN:
        return parseCall();
      case Token::Type::NUMBER:
        return parseNumber();
      case Token::Type::ID:
        return parseID();
      case Token::Type::STRING:
        return parseString();
      default:
        m_lexer.invalidType();
    }
  }

  std::shared_ptr<AST::Number> Parser::parseNumber() {
    auto number = std::static_pointer_cast<Token::Number>(m_lexer.token(Token::Type::NUMBER));
    auto ast = std::make_shared<AST::Number>(number->value);
    ast->loc = number->loc;
    return ast;
  }

  std::shared_ptr<AST::ID> Parser::parseID() {
    auto id = std::static_pointer_cast<Token::ID>(m_lexer.token(Token::Type::ID));

    unsigned uid;
    auto it = std::find(m_ast->strings.begin(), m_ast->strings.end(), id->name);
    if (it != m_ast->strings.end()) {
      uid = it - m_ast->strings.begin();
    } else {
      uid = str_uid++;
      m_ast->strings.push_back(id->name);
    }

    auto ast = std::make_shared<AST::ID>(m_ast->strings[uid], uid);
    ast->loc = id->loc;
    return ast;
  }

  std::shared_ptr<AST::String> Parser::parseString() {
    auto string = std::static_pointer_cast<Token::String>(m_lexer.token(Token::Type::STRING));

    int uid;
    auto it = std::find(m_ast->strings.begin(), m_ast->strings.end(), string->value);
    if (it != m_ast->strings.end()) {
      uid = it - m_ast->strings.begin();
    } else {
      uid = str_uid++;
      m_ast->strings.push_back(string->value);
    }

    auto ast =  std::make_shared<AST::String>(m_ast->strings[uid], uid);
    ast->loc = string->loc;
    return ast;
  }

  std::shared_ptr<AST::Call> Parser::parseCall(void) {
    auto start = m_lexer.token(Token::Type::L_PAREN)->loc.start;

    auto call = std::make_shared<AST::Call>();

    while (m_lexer.token()->type != Token::Type::R_PAREN) {
      call->arguments.push_back(parseFactor());
    }

    auto end = m_lexer.token(Token::Type::R_PAREN)->loc.end;
    call->loc = { start, end };

    return call;
  }

}
