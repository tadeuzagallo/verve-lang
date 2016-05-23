#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {
  Parser::Parser(Lexer &lexer) :
    m_lexer(lexer)
  {
    m_environment = std::make_shared<Environment>();
    m_scope = std::make_shared<ParseScope>();
  }

  AST::ProgramPtr Parser::parse() {
    auto program = AST::createProgram();
    program->body = AST::createBlock();
    while(!next(Token::END)) {
      auto node = parseDecl();
      if (node) {
        program->body->nodes.push_back(node);
      }
    }
    return program;
  }

  AST::NodePtr Parser::parseDecl() {
    if (next(Token::ID)) {
      if (skip("interface")) {
        return parseInterface();
      } else if (skip("implementation")) {
        return parseImplementation();
      } else if (skip("extern")) {
        parseExtern();
        return nullptr;
      }
    }

    return parseFactor();
  }

  AST::BlockPtr Parser::parseInterface() {
    auto interface = new TypeInterface();
    interface->name = token(Token::ID).string();
    setType(interface->name, interface);

    pushTypeScope();
    parseGenerics(interface->generics);

    auto block = AST::createBlock();

    match('{');
    while (!skip('}')) {
      if (skip("virtual")) {
        auto virtualFunction = parseVirtual();
        virtualFunction->interface = interface;
      } else {
        auto fn = parseFunction();
        block->nodes.push_back(fn);
      }
    }

    popTypeScope();

    return block;
  }

  AST::BlockPtr Parser::parseImplementation() {
    auto implementation = new TypeImplementation();
    auto interfaceName = token(Token::ID).string();

    auto interface = getType<TypeInterface *>(interfaceName);
    assert(interface);

    interface->implementations.push_back(implementation);

    pushTypeScope();

    match('<');
    unsigned i = 0;
    do {
      setType(interface->generics[i++], parseType());
    } while(!skip('>'));

    assert(i == interface->generics.size());

    match('{');
    auto block = AST::createBlock();
    while(!skip('}')) {
      if (skip("extern")) {
        parseExtern();
      } else {
        block->nodes.push_back(parseTypelessFunction());
      }
    }

    popTypeScope();

    return block;
  }

  AST::NodePtr Parser::parseFactor() {
    if (next(Token::NUMBER)) {
      return parseNumber();
    } else if (next(Token::STRING)) {
      return parseString();
    } else if (next(Token::ID)) {
      if (skip("if")) {
        return parseIf();
      } else {
        return parseIdentifierFunctionOrCall();
      }
    }
    m_lexer.invalidToken();
  }

  TypeFunction *Parser::parseVirtual() {
    auto prototype = parsePrototype();
    prototype->isVirtual = true;
    setType(prototype->name, prototype);

    return prototype;
  }

  TypeFunction *Parser::parseExtern() {
    auto prototype = parsePrototype();
    prototype->isExternal = true;
    setType(prototype->name, prototype);

    return prototype;
  }

  TypeFunction *Parser::parsePrototype() {
    auto fnType = new TypeFunction();
    fnType->name = token(Token::ID).string();

    if (next('<')) {
      parseGenerics(fnType->generics);
    }

    match('(');
    while (!next(')')) {
      fnType->types.push_back(parseType());
      if (!skip(',')) break;
    }
    match(')');

    match(':');
    fnType->returnType = parseType();

    return fnType;
  }

  AST::FunctionPtr Parser::parseTypelessFunction() {
    auto fn = AST::createFunction();
    fn->name = parseIdentifier();

    pushScope();

    match('(');
    unsigned i = 0;
    while (!next(')')) {
      auto arg = AST::createFunctionParameter();
      arg->name = token(Token::ID).string();
      arg->uid = i++;
      fn->parameters.push_back(arg);
      m_scope->set(arg->name, arg);

      if (!skip(',')) break;
    }
    match(')');
    fn->body = parseBody();
    fn->needsScope = m_scope->isRequired;
    fn->capturesScope = m_scope->capturesScope;

    popScope();

    return fn;
  }

  AST::FunctionPtr Parser::parseFunction() {
    auto start = token().loc;

    auto fn = AST::createFunction();
    auto fnType = new TypeFunction();
    fn->name = parseIdentifier();
    fnType->name = fn->name->name;

    setType(fnType->name, fnType);

    pushScope();

    if (next('<')) {
      parseGenerics(fnType->generics);
    }

    if (!skip('(')) goto rewind;
    if (!parseFunctionParams(fn->parameters, fnType->types)) goto rewind;
    if (!skip(':')) goto rewind;

    fnType->returnType = parseType();

    fn->body = parseBody();
    fn->needsScope = m_scope->isRequired;
    fn->capturesScope = m_scope->capturesScope;
    //Type::checkReturnType(fnType, fn->body, m_environment);

    popScope();

    return fn;

rewind:
    popScope();
    m_lexer.rewind(start);
    return nullptr;
  }

  AST::IfPtr Parser::parseIf() {
    auto iff = AST::createIf();

    match('(');
    iff->condition = parseFactor();
    match(')');

    iff->ifBody = parseFactorOrBody();

    if (skip("else")) {
      iff->elseBody = parseFactorOrBody();
    }

    return iff;
  }

  AST::BlockPtr Parser::parseFactorOrBody() {
    if (next('{')) {
      return parseBody();
    } else {
      auto block = AST::createBlock();
      block->nodes.push_back(parseFactor());
      return block;
    }
  }

  AST::BlockPtr Parser::parseBody() {
    auto block = AST::createBlock();
    match('{');
    while (!skip('}')) {
      block->nodes.push_back(parseFactor());
    }
    return block;
  }

  bool Parser::parseFunctionParams(
      std::vector<AST::FunctionParameterPtr> &params,
      std::vector<Type *> &types)
  {
    unsigned i = 0;
    while (!next(')')) {

      if (!next(Token::ID)) goto fail;
      auto param = AST::createFunctionParameter(token().loc);
      param->name = token(Token::ID).string();
      param->uid = i++;
      params.push_back(param);
      m_scope->set(param->name, param);

      if (!skip(':')) goto fail;

      auto type = parseType();
      types.push_back(type);
      setType(param->name, type);

      if (!skip(',')) break;
    }

    match(')');

    return true;

fail:
    return false;
  }

  void Parser::parseGenerics(std::vector<std::string> &generics) {
    match('<');

    do {
      auto name = token(Token::ID).string();
      generics.push_back(name);

      setType(name, new GenericType(std::move(name)));
    } while (skip(','));

    match('>');
  }

  AST::NodePtr Parser::parseIdentifierFunctionOrCall() {
    AST::NodePtr callee = parseFunction();
    if (!callee) {
      callee = parseIdentifier(true);
    }
    return parseCall(callee);
  }

  AST::NodePtr Parser::parseCall(AST::NodePtr callee) {
    if (!skip('(')) {
      return callee;
    }
    auto call = AST::createCall();
    call->callee = callee;
    while (!next(')')) {
      call->arguments.push_back(parseFactor());
      if (!skip(',')) break;
    }
    match(')');
    return parseCall(call);
  }

  // Base nodes

  AST::IdentifierPtr Parser::parseIdentifier(bool checkScope) {
    auto loc = token().loc;
    auto name = token(Token::ID).string();

    if (checkScope) {
      auto var = m_scope->get(name);
      if (var && var->type == AST::Type::FunctionParameter) {
        auto param = AST::asFunctionParameter(var);
        auto ident = AST::createFunctionParameter(loc);
        ident->uid = param->uid;
        ident->name = name;

        ParseScopePtr scope;
        if ((scope = m_scope->scopeFor(name)) != m_scope) {
          param->isCaptured = true;
          scope->isRequired = true;
          m_scope->capturesScope = true;
        } else {
          return ident;
        }
      }
    }

    auto identifier = AST::createIdentifier();
    identifier->name = name;
    return identifier;
  }

  AST::NumberPtr Parser::parseNumber() {
    auto number = AST::createNumber();
    number->value = token(Token::NUMBER).number();
    return number;
  }

  AST::StringPtr Parser::parseString() {
    auto str = AST::createString();
    str->name = token(Token::STRING).string();
    return str;
  }

  Type *Parser::parseType() {
    if (skip('(')) {
      auto type = new TypeFunction();
      while (!next(')')) {
        type->types.push_back(parseType());
        if (!skip(',')) break;
      }
      match(')');
      match(':');
      type->returnType = parseType();
      return type;
    } else {
      auto type = getType(token(Token::ID).string());
      if (!skip('<')) {
        return type;
      }

      auto dti = new DataTypeInstance();
      dti->dataType = dynamic_cast<DataType *>(type);
      assert(dti->dataType);
      do {
        dti->types.push_back(parseType());
        if (!next('>')) break;
      } while(!next('>'));
      match('>');
      assert(dti->types.size() == dti->dataType->length);
      return dti;
    }
  }

  // Type helpers

  void Parser::setType(std::string &typeName, Type *type) {
    m_environment->types[typeName] = type;
  }

  template <typename T>
  T Parser::getType(std::string typeName) {
    auto env = m_environment;
    while (env) {
      auto it = env->types.find(typeName);
      if (it != env->types.end()) {
        return dynamic_cast<T>(it->second);
      }
      env = env->parent;
    }
    return nullptr;
  }

  void Parser::pushTypeScope() {
    auto env = std::make_shared<Environment>();
    env->parent = m_environment;
    m_environment = env;
  }

  void Parser::popTypeScope() {
    m_environment = m_environment->parent;
  }

  // Var helpers

  void Parser::pushScope() {
    m_scope = m_scope->create();

    pushTypeScope();
  }

  void Parser::popScope() {
    m_scope = m_scope->restore();

    popTypeScope();
  }

  // Lexer aliases

  inline Token &Parser::token() {
    return m_lexer.token();
  }

  inline Token &Parser::token(Token::Type t) {
    return m_lexer.token(t);
  }

  inline bool Parser::next(char c) {
    return m_lexer.next(c);
  }

  inline bool Parser::skip(char c) {
    return m_lexer.skip(c);
  }

  void Parser::match(char c) {
    m_lexer.match(c);
  }

  inline bool Parser::next(std::string str) {
    return m_lexer.token().type == Token::ID && m_lexer.token().string() == str;
  }

  inline bool Parser::skip(std::string str) {
    if (next(str)) {
      token(Token::ID);
      return true;
    } else {
      return false;
    }
  }

  inline bool Parser::next(Token::Type t) {
    return m_lexer.token().type == t;
  }
}
