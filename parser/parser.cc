#include "parser.h"

#include "lexer.h"
#include "token.h"
#include "type_checker.h"

namespace ceos {
  Parser::Parser(Lexer &lexer) :
    m_lexer(lexer)
  {
    m_environment = std::make_shared<Environment>();
    m_scope = std::make_shared<ParseScope>();

    setType("Int", new BasicType("Int"));
    setType("Char", new BasicType("Char"));
    setType("Float", new BasicType("Float"));
    setType("Void", new BasicType("Void"));

    auto List = new DataType("List");
    List->length = 1;
    setType("List", List);

    auto String = new DataTypeInstance();
    String->dataType = List;
    String->types.push_back(getType("Char"));
    setType("String", String);
  }

  AST::ProgramPtr Parser::parse() {
    auto program = AST::createProgram(Loc{0, 0});
    program->body = AST::createBlock(Loc{0, 0});
    m_blockStack.push_back(program->body);
    while(!next(Token::END)) {
      auto node = parseDecl();
      if (node) {
        program->body->nodes.push_back(node);
      }
    }
    m_blockStack.pop_back();
    return program;
  }

  AST::NodePtr Parser::parseDecl() {
    if (next(Token::ID)) {
      if (skip("interface")) {
        return parseInterface();
      } else if (skip("implementation")) {
        return parseImplementation();
      } else if (skip("type")) {
        parseTypeDecl();
        return nullptr;
      } else if (skip("extern")) {
        parseExtern();
        return nullptr;
      }
    }

    return parseExpr();
  }

  void Parser::parseTypeDecl() {
    auto type = new EnumType();
    auto tag = 0u;

    type->name = token(Token::ID).string();

    setType(type->name, type);

    match('{');
    while (!skip('}')) {
      parseTypeConstructor(tag++, type);
    }
  }

  void Parser::parseTypeConstructor(unsigned tag, EnumType *owner) {
    auto ctor = new TypeConstructor();
    ctor->type = new TypeFunction();

    ctor->type->name = token(Token::ID).string();
    ctor->type->returnType = owner;

    if (ctor->type->name == owner->name) {
      // keep track of the enclosing enum that is going to be shadowed in the type scope
      ctor->owner = owner;
    }

    setType(ctor->type->name, ctor);

    ctor->tag = tag;

    match('(');
    while (!next(')')) {
      ctor->size++;
      ctor->type->types.push_back(parseType());

      if (!skip(',')) {
        break;
      }
    }
    match(')');
  }

  AST::BlockPtr Parser::parseInterface() {
    auto interface = new TypeInterface();
    interface->name = token(Token::ID).string();
    setType(interface->name, interface);

    auto declScope = m_environment;
    pushTypeScope();

    match('<');
    interface->genericTypeName = token(Token::ID).string();
    match('>');

    setType(interface->genericTypeName, new GenericType(interface->genericTypeName));

    auto block = AST::createBlock(token().loc);

    match('{');
    while (!skip('}')) {
      if (skip("virtual")) {
        auto virtualFunction = parseVirtual(declScope);
        virtualFunction->interface = interface;
      } else if (skip("fn")) {
        auto fn = parseFunction();
        block->nodes.push_back(fn);
      } else {
        match('}');
        break;
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
    implementation->interface = interface;

    auto declScope = m_environment;
    pushTypeScope();

    match('<');
    implementation->type = parseType();
    setType(interface->genericTypeName, implementation->type);
    match('>');

    auto implementationSuffix = implementation->type->toString();

    match('{');
    auto block = AST::createBlock(token().loc);
    while(!skip('}')) {
      if (skip("extern")) {
        parseExtern(declScope, implementationSuffix);
      } else if (skip("fn")) {
        auto fn = parseTypelessFunction(implementationSuffix, declScope);
        block->nodes.push_back(fn);
      } else {
        match('}');
        break;
      }
    }

    popTypeScope();

    return block;
  }

  AST::NodePtr Parser::parseExpr(int precedence) {
    auto lhs = parseFactor();
    int prec;
    while ((prec = Token::precedence(token())) >= precedence) {
      auto binop = AST::createBinaryOperation(token().loc);
      binop->op = token(Token::BASIC).number();
      binop->lhs = lhs;
      binop->rhs = parseExpr(prec + 1);
      lhs = binop;
    }
    return lhs;
  }

  AST::NodePtr Parser::parseFactor() {
    if (skip('(')) {
      auto expr = parseExpr();
      match(')');
      return expr;
    } else if (skip('[')) {
      return parseList();
    } else if (Token::isUnaryOperator(token())) {
      auto prec = Token::precedence(token());

      auto unop = AST::createUnaryOperation(token().loc);
      unop->op = token(Token::BASIC).number();
      unop->operand = parseExpr(prec);

      return unop;
    } else if (next(Token::NUMBER)) {
      return parseNumber();
    } else if (next(Token::STRING)) {
      return parseString();
    } else if (next(Token::ID)) {
      if (skip("if")) {
        return parseIf();
      } else if (skip("let")) {
        return parseLet();
      } else if (skip("match")) {
        return parseMatch();
      } else {
        return parseIdentifierFunctionOrCall();
      }
    }
    m_lexer.invalidToken();
  }

  TypeFunction *Parser::parseVirtual(std::shared_ptr<Environment> declScope) {
    auto prototype = parsePrototype();
    prototype->isVirtual = true;
    setType(prototype->name, prototype, declScope);

    return prototype;
  }

  TypeFunction *Parser::parseExtern(std::shared_ptr<Environment> scope, std::string implementationSuffix) {
    auto prototype = parsePrototype(implementationSuffix);
    prototype->isExternal = true;
    setType(prototype->name, prototype, scope);

    return prototype;
  }

  TypeFunction *Parser::parsePrototype(std::string implementationSuffix) {
    auto fnType = new TypeFunction();
    fnType->name = token(Token::ID).string() + implementationSuffix;

    parseGenerics(fnType->generics);

    match('(');
    while (!next(')')) {
      fnType->types.push_back(parseType());
      if (!skip(',')) break;
    }
    match(')');

    match(TUPLE_TOKEN('-', '>'));
    fnType->returnType = parseType();

    return fnType;
  }

  AST::FunctionPtr Parser::parseTypelessFunction(std::string implementationName, std::shared_ptr<Environment> declScope) {
    auto fn = AST::createFunction(token().loc);
    fn->name = token(Token::ID).string();

    auto fnType = getType<TypeFunction *>(fn->name);
    fn->name += implementationName;
    setType(fn->name, fnType, declScope);

    pushScope();

    match('(');
    unsigned i = 0;
    while (!next(')')) {
      auto arg = AST::createFunctionParameter(token().loc);
      arg->name = token(Token::ID).string();
      arg->index = i++;
      fn->parameters.push_back(arg);
      m_scope->set(arg->name, arg);

      setType(arg->name, fnType->types[arg->index]);

      if (!skip(',')) break;
    }
    match(')');

    fn->body = AST::createBlock(token().loc);
    fn->body->env = m_environment;
    m_blockStack.push_back(fn->body);
    parseBody(fn->body);
    m_blockStack.pop_back();

    fn->needsScope = m_scope->isRequired;
    fn->capturesScope = m_scope->capturesScope;
    TypeChecker::checkReturnType(fnType->returnType, fn->body, m_environment.get(), m_lexer);

    popScope();

    return fn;
  }

  AST::FunctionPtr Parser::parseFunction() {
    auto start = token().loc;

    auto fn = AST::createFunction(start);
    auto fnType = new TypeFunction();
    fn->name = token(Token::ID).string();
    fnType->name = fn->name;

    auto env = m_environment;

    pushScope();

    parseGenerics(fnType->generics);

    parseFunctionParams(fn->parameters, fnType->types);

    match(TUPLE_TOKEN('-', '>'));
    setType(fnType->name, fnType, env);
    fnType->returnType = parseType();

    fn->body = AST::createBlock(token().loc);
    fn->body->env = m_environment;
    m_blockStack.push_back(fn->body);
    parseBody(fn->body);
    m_blockStack.pop_back();

    fn->needsScope = m_scope->isRequired;
    fn->capturesScope = m_scope->capturesScope;
    TypeChecker::checkReturnType(fnType->returnType, fn->body, m_environment.get(), m_lexer);

    popScope();

    return fn;
  }

  AST::IfPtr Parser::parseIf() {
    auto iff = AST::createIf(token().loc);

    iff->condition = parseExpr();

    iff->ifBody = parseExprOrBody();

    if (skip("else")) {
      iff->elseBody = parseExprOrBody();
    }

    return iff;
  }

  AST::LetPtr Parser::parseLet() {
    auto let = AST::createLet(token().loc);

    pushScope();
    m_scope->escapes = false;

    let->block = AST::createBlock(token().loc);
    let->block->env = m_environment;

    while (!next('{')) {
      auto name = token(Token::ID).string();

      auto loc = token().loc;
      if (skip('(')) {
        auto ctor = getType<TypeConstructor *>(name);
        assert(ctor);

        auto tagTest = AST::createObjectTagTest(token().loc);
        tagTest->tag = ctor->tag;
        let->block->nodes.push_back(tagTest);

        auto offset = 0u;
        while (!next(')')) {
          auto load = AST::createObjectLoad(token().loc);
          load->offset = offset++;
          load->constructorName = name;

          auto store = AST::createStackStore(token().loc);
          store->slot = m_blockStack.back()->stackSlots++;
          store->value = load;
          let->stores.push_back(store);

          auto stackLoad = AST::createStackLoad(token().loc);
          stackLoad->slot = store->slot;
          stackLoad->value = load;
          stackLoad->name = token(Token::ID).string();
          stackLoad->isCaptured = true;
          let->loads.push_back(stackLoad);

          setType(stackLoad->name, ctor->type->types[load->offset]);

          if (!skip(',')) break;
        }
        match(')');

        match('=');

        // assert(loads.size() == ctor->type->types.size()); ??

        auto object = parseExpr();
        tagTest->object = object;
        TypeChecker::checkPatternMatch(ctor, object, loc, m_environment.get(), m_lexer);
        for (auto load : let->loads) {
          AST::asObjectLoad(load->value)->object = object;
          m_scope->set(load->name, load);
        }
      } else {
        match('=');

        auto store = AST::createStackStore(token().loc);
        store->slot = m_blockStack.back()->stackSlots++;
        store->value = parseExpr();
        let->block->nodes.push_back(store);

        auto load = AST::createStackLoad(token().loc);
        load->slot = store->slot;
        load->value = store->value;
        m_scope->set(name, load);
      }
    }

    parseBody(let->block);

    if (m_scope->isRequired) {
      m_scope->parent()->isRequired = true;
    }
    if (m_scope->capturesScope) {
      m_scope->parent()->capturesScope = true;
    }
    popScope();

    return let;
  }

  AST::MatchPtr Parser::parseMatch() {
    auto match = AST::createMatch(token().loc);
    match->value = parseExpr();

    this->match('{');
    while (!skip('}')) {
      pushScope();

      auto kase = AST::createCase(token().loc);
      kase->pattern = parsePattern(match->value);
      this->match(TUPLE_TOKEN('=', '>'));
      kase->body = parseExprOrBody();
      kase->body->env = m_environment;
      match->cases.push_back(kase);

      popScope();
    }

    return match;
  }

  AST::PatternPtr Parser::parsePattern(AST::NodePtr value) {
    auto pattern = AST::createPattern(token().loc);

    auto name = token(Token::ID).string();
    auto ctor = getType<TypeConstructor *>(name);
    assert(ctor);

    pattern->tag = ctor->tag;

    match('(');
    auto offset = 0u;
    while (!next(')')) {
      auto load = AST::createObjectLoad(token().loc);
      load->offset = offset++;
      load->object = value;
      load->constructorName = name;

      auto name = token(Token::ID).string();

      auto store = AST::createStackStore(token().loc);
      store->slot = m_blockStack.back()->stackSlots++;
      store->value = load;

      pattern->stores.push_back(store);

      auto stackLoad = AST::createStackLoad(token().loc);
      stackLoad->slot = store->slot;
      stackLoad->value = load;
      m_scope->set(name, stackLoad);

      if (!skip(',')) break;
    }
    match(')');

    return pattern;
  }

  AST::BlockPtr Parser::parseExprOrBody() {
    if (next('{')) {
      return parseBody();
    } else {
      auto block = AST::createBlock(token().loc);
      block->nodes.push_back(parseExpr());
      return block;
    }
  }

  AST::BlockPtr Parser::parseBody(AST::BlockPtr block) {
    if (block == nullptr) {
      block = AST::createBlock(token().loc);
    }

    match('{');
    while (!skip('}')) {
      block->nodes.push_back(parseExpr());
    }
    return block;
  }

  void Parser::parseFunctionParams(
      std::vector<AST::FunctionParameterPtr> &params,
      std::vector<Type *> &types)
  {
    match('(');

    unsigned i = 0;
    while (!next(')')) {
      auto param = AST::createFunctionParameter(token().loc);
      param->name = token(Token::ID).string();
      param->index = i++;
      params.push_back(param);
      m_scope->set(param->name, param);

      match(':');

      auto type = parseType();
      types.push_back(type);
      setType(param->name, type);

      if (!skip(',')) break;
    }

    match(')');
  }

  void Parser::parseGenerics(std::vector<std::string> &generics) {
    if (!skip('<')) {
      return;
    }

    do {
      auto name = token(Token::ID).string();
      generics.push_back(name);

      setType(name, new GenericType(std::move(name)));
    } while (skip(','));

    return match('>');
  }

  AST::NodePtr Parser::parseIdentifierFunctionOrCall() {
    AST::NodePtr callee;

    if (skip("fn")) {
      callee = parseFunction();
    } else {
      callee = parseIdentifier(true);
    }
    return parseCall(callee);
  }

  AST::NodePtr Parser::parseCall(AST::NodePtr callee) {
    auto loc = token().loc;
    if (!skip('(')) {
      return callee;
    }
    auto call = AST::createCall(loc);
    call->callee = callee;
    while (!next(')')) {
      call->arguments.push_back(parseExpr());
      if (!skip(',')) break;
    }
    match(')');

    pushTypeScope();
    TypeChecker::checkCall(call, m_environment.get(), m_lexer);
    popTypeScope();

    return parseCall(call);
  }

  // Base nodes

  AST::NodePtr Parser::parseIdentifier(bool checkScope) {
    auto loc = token().loc;
    auto name = token(Token::ID).string();

    if (checkScope) {
      auto var = m_scope->get(name);
      if (var && (var->type == AST::Type::FunctionParameter || var->type == AST::Type::StackLoad)) {
        ParseScopePtr scope;
        if ((scope = m_scope->scopeFor(name)) != m_scope) {
          bool shouldCapture = false;
          auto s = m_scope;
          while (s != scope) {
            if (s->escapes) {
              shouldCapture = true;
              break;
            }
            s = s->parent();
          }

          if (shouldCapture) {
            if (var->type == AST::Type::FunctionParameter) {
              AST::asFunctionParameter(var)->isCaptured = true;
            } else if (var->type == AST::Type::StackLoad) {
              AST::asStackLoad(var)->isCaptured = true;
            }
            scope->isRequired = true;
            m_scope->capturesScope = true;
            goto ident;
          }
        }
      }
      if (var) return var;
    }
ident:

    auto identifier = AST::createIdentifier(loc);
    identifier->name = name;
    return identifier;
  }

  AST::NumberPtr Parser::parseNumber() {
    auto number = AST::createNumber(token().loc);
    number->value = token(Token::NUMBER).number();
    return number;
  }

  AST::StringPtr Parser::parseString() {
    auto str = AST::createString(token().loc);
    str->name = token(Token::STRING).string();
    return str;
  }

  AST::ListPtr Parser::parseList() {
    auto list = AST::createList(token().loc);
    while (!next(']')) {
      list->items.push_back(parseExpr());
      if (!skip(',')) break;
    }
    match(']');
    return list;
  }

  Type *Parser::parseType() {
    if (skip('(')) {
      auto type = new TypeFunction();
      while (!next(')')) {
        type->types.push_back(parseType());
        if (!skip(',')) break;
      }
      match(')');
      match(TUPLE_TOKEN('-', '>'));
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

  void Parser::setType(std::string typeName, Type *type, std::shared_ptr<Environment> env) {
    (env ?: m_environment)->types[typeName] = type;
  }

  template <typename T>
  T Parser::getType(std::string typeName) {
    return dynamic_cast<T>(m_environment->get(typeName));
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

  inline bool Parser::next(int c) {
    return m_lexer.next(c);
  }

  inline bool Parser::skip(int c) {
    return m_lexer.skip(c);
  }

  void Parser::match(int c) {
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
