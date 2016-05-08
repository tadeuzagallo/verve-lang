#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {

  std::shared_ptr<AST::Program> Parser::parse(void) {
    m_lexer.nextToken();
    m_ast = std::make_shared<AST::Program>();
    m_ast->loc.start = m_lexer.token()->loc.start;
    m_ast->body = parseBlock(Token::Type::END);
    m_ast->body->needsScope = false;
    return m_ast;
  }

  std::shared_ptr<AST::Block> Parser::parseBlock(Token::Type delim) {
    auto block = std::make_shared<AST::Block>();
    while (m_lexer.token()->type != delim) {
      std::shared_ptr<AST> node = parseFactor();
      block->nodes.push_back(node);
    }
    block->needsScope = m_scope->isRequired;
    block->capturesScope = m_scope->capturesScope;

    return block;
  }

  std::shared_ptr<AST> Parser::parseFactor() {
    switch (m_lexer.token()->type) {
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

  std::shared_ptr<AST> Parser::parseIf() {
    auto _if = std::make_shared<AST::If>();

    m_lexer.ensure(Token::Type::L_PAREN);
    _if->condition = parseFactor();
    m_lexer.ensure(Token::Type::R_PAREN);

    if (m_lexer.token()->type == Token::Type::L_BRACE) {
      m_lexer.ensure(Token::Type::L_BRACE);
      _if->ifBody = parseBlock(Token::Type::R_BRACE);
      m_lexer.ensure(Token::Type::R_BRACE);
    } else {
      auto ifBody = std::make_shared<AST::Block>();
      ifBody->nodes.push_back(parseFactor());
      _if->ifBody = ifBody;
    }

    if (m_lexer.token()->type == Token::Type::ID) {
      auto maybeElse = std::static_pointer_cast<Token::ID>(m_lexer.token());
      if (maybeElse->name == "else") {
        m_lexer.ensure(Token::Type::ID);

        if (m_lexer.token()->type == Token::Type::L_BRACE) {
          m_lexer.ensure(Token::Type::L_BRACE);
          _if->elseBody = parseBlock(Token::Type::R_BRACE);
          m_lexer.ensure(Token::Type::R_BRACE);
        } else {
          auto elseBody = std::make_shared<AST::Block>();
          elseBody->nodes.push_back(parseFactor());
          _if->elseBody = elseBody;
        }
      }
    }

    return _if;
  }

  std::shared_ptr<AST::Number> Parser::parseNumber() {
    auto number = std::static_pointer_cast<Token::Number>(m_lexer.token(Token::Type::NUMBER));
    auto ast = std::make_shared<AST::Number>(number->value);
    ast->loc = number->loc;
    return ast;
  }

  std::shared_ptr<AST> Parser::parseID() {
    auto id = std::static_pointer_cast<Token::ID>(m_lexer.token(Token::Type::ID));

    if (id->name == "if") {
      return parseIf();
    }

    std::shared_ptr<AST> ast, ref;
    if ((ref = m_scope->get(id->name, false)) != nullptr && ref->type == AST::Type::FunctionArgument) {
      ast = ref;
    } else {
      unsigned uid;
      auto it = std::find(m_ast->strings.begin(), m_ast->strings.end(), id->name);
      if (it != m_ast->strings.end()) {
        uid = it - m_ast->strings.begin();
      } else {
        uid = str_uid++;
        m_ast->strings.push_back(id->name);
      }

      ast = std::make_shared<AST::ID>(m_ast->strings[uid], uid);
      ast->loc = id->loc;

      if ((ref = m_scope->get(id->name)) && !m_scope->isInCurrentScope(id->name)) {
        if (ref->type == AST::Type::FunctionArgument) {
          AST::asFunctionArgument(ref)->isCaptured = true;
        }
        m_scope->scopeFor(id->name)->isRequired = true;
        m_scope->capturesScope = true;
      }
    }

    while (true) {
      if (m_lexer.token()->type == Token::Type::TYPE) {
        ast = parseTypeInfo(std::move(ast));
      } else if (m_lexer.token()->type == Token::Type::L_PAREN) {
        ast = parseCall(std::move(ast));
      } else if (m_lexer.token()->type == Token::Type::L_BRACE) {
        assert(ast->type == AST::Type::Call);
        auto call = AST::asCall(ast);
        ast = parseFunction(std::move(call));
      } else {
        break;
      }
    }

    return ast;
  }

  std::shared_ptr<AST::Function> Parser::parseFunction(std::shared_ptr<AST::Call> &&call) {
    assert(call->callee->type == AST::Type::ID);

    auto fn = std::make_shared<AST::Function>();
    fn->name = AST::asID(call->callee);

    m_scope->set(fn->name->name, fn);
    m_scope->isRequired = true;

    m_scope = m_scope->create();

    unsigned i = 0;
    for (auto arg : call->arguments) {
      std::string argName;
      if (arg->type == AST::Type::ID) {
        argName = AST::asID(arg)->name;
      } else if (arg->type == AST::Type::FunctionArgument) {
        argName = AST::asFunctionArgument(arg)->name;
      } else {
        perror("Can't handle argument type on function declaration");
        throw;
      }

      auto fnArg = std::make_shared<AST::FunctionArgument>(argName, i++);
      fn->arguments.push_back(fnArg);

      m_scope->set(argName, fnArg);
    }

    m_lexer.ensure(Token::Type::L_BRACE);
    fn->body = parseBlock(Token::Type::R_BRACE);
    fn->loc.start = fn->name->loc.start;
    fn->loc.end = m_lexer.token(Token::Type::R_BRACE)->loc.end;

    m_scope = m_scope->restore();

    return fn;
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

  std::shared_ptr<AST::Call> Parser::parseCall(std::shared_ptr<AST> &&callee) {
    auto start = callee->loc.start;

    m_lexer.ensure(Token::Type::L_PAREN);

    auto call = std::make_shared<AST::Call>();
    call->callee = callee;

    while (m_lexer.token()->type != Token::Type::R_PAREN) {
      call->arguments.push_back(parseFactor());
      if (m_lexer.token()->type != Token::Type::R_PAREN) {
        m_lexer.ensure(Token::Type::COMMA);
      }
    }

    auto end = m_lexer.token(Token::Type::R_PAREN)->loc.end;
    call->loc = { start, end };

    return call;
  }

  std::shared_ptr<AST::TypeInfo> Parser::parseTypeInfo(std::shared_ptr<AST> &&target) {
    m_lexer.ensure(Token::Type::TYPE);

    auto typeInfo = std::make_shared<AST::TypeInfo>();
    typeInfo->target = std::move(target);
    typeInfo->type = parseID();
    return typeInfo;
  }

}
