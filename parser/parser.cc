#include "parser.h"

#include "lexer.h"
#include "token.h"
#include "type_checker.h"

#include "utils/file.h"

namespace ceos {
  Parser::Parser(Lexer &lexer, std::string dirname, std::string ns) :
    m_lexer(lexer), m_dirname(dirname), m_ns(ns)
  {
    m_environment = std::make_shared<Environment>();
    m_scope = std::make_shared<ParseScope>();

    setType("int", new BasicType("int"));
    setType("char", new BasicType("char"));
    setType("float", new BasicType("float"));
    setType("void", new BasicType("void"));

    auto list = new EnumType();
    list->name = "list";
    list->generics.push_back("T");
    setType("list", list);

    auto string = new DataTypeInstance();
    string->dataType = list;
    string->types.push_back(getType("char"));
    setType("string", string);
  }

  AST::ProgramPtr Parser::parse() {
    auto program = AST::createProgram(Loc{0, 0});
    program->body = AST::createBlock(Loc{0, 0});
    m_blockStack.push_back(program->body);
    while(!next(Token::END)) {
      while (skip("import")) {
        auto node = parseImport();
        if (node) {
          program->body->nodes.push_back(node);
        }
      }
      auto node = parseDecl();
      if (node) {
        pushTypeScope();
        TypeChecker::check(node, m_environment, m_lexer);
        popTypeScope();
        program->body->nodes.push_back(node);
      }
    }
    m_blockStack.pop_back();
    m_ast = program;
    return program;
  }

  AST::NodePtr Parser::parseImport() {
    bool importAll = false;
    std::vector<std::string> imports;
    if (skip('*')) {
      importAll = true;
    } else {
      match('{');
      while (!next('}')) {
        imports.push_back(token(Token::LCID).string());
        if (!skip(',')) break;
      }
      match('}');
    }

    match("from");

    auto path = token(Token::STRING).string();

    std::string ns = "";
    if (skip("as")) {
      ns = token(Token::UCID).string();
    }

    auto parser = parseFile(path, m_dirname, ns);

    if (importAll) {
      for (auto it : parser.m_environment->types) {
        m_environment->types[namespaced(ns, it.first)] = it.second;
      }
    } else {
      for (auto import : imports) {
        m_environment->types[import] = parser.m_environment->types[import];
      }
    }

    return parser.m_ast->body;
  }

  AST::NodePtr Parser::parseDecl() {
    if (next(Token::LCID)) {
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

    type->name = token(Token::LCID).string();

    setType(type->name, type);

    parseGenerics(type->generics);

    match('{');
    while (!skip('}')) {
      parseTypeConstructor(tag++, type);
    }
  }

  void Parser::parseTypeConstructor(unsigned tag, EnumType *owner) {
    auto ctor = new TypeConstructor();
    ctor->type = new TypeFunction();

    ctor->type->name = token(Token::UCID).string();
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
    interface->name = token(Token::LCID).string();
    setType(interface->name, interface);

    auto declScope = m_environment;
    pushTypeScope();

    match('<');
    interface->genericTypeName = token(Token::LCID).string();
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
    auto interfaceName = token(Token::LCID).string();

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
    } else if (next(Token::LCID)) {
      if (skip("if")) {
        return parseIf();
      } else if (skip("let")) {
        return parseLet();
      } else if (skip("match")) {
        return parseMatch();
      } else {
        return parseIdentifierFunctionOrCall();
      }
    } else if (next(Token::UCID)) {
      auto ucid = token(Token::UCID).string();
      if (skip('#')) {
        return parseCall(parseIdentifier(ucid));
      } else {
        return parseConstructor(ucid);
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
    fnType->name = token(Token::LCID).string() + implementationSuffix;

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
    fn->name = token(Token::LCID).string();

    auto fnType = getType<TypeFunction *>(fn->name);
    fn->name += implementationName;
    setType(fn->name, fnType, declScope);

    for (auto g : fnType->generics) {
      setType(g, new GenericType(g));
    }

    pushScope();

    match('(');
    unsigned i = 0;
    while (!next(')')) {
      auto arg = AST::createFunctionParameter(token().loc);
      arg->name = token(Token::LCID).string();
      arg->index = i++;
      fn->parameters.push_back(arg);
      m_scope->set(arg->name, arg);

      auto type = fnType->types[arg->index];
      setType(arg->name, type);

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

    popScope();

    return fn;
  }

  AST::FunctionPtr Parser::parseFunction() {
    auto start = token().loc;

    auto fn = AST::createFunction(start);
    auto fnType = new TypeFunction();
    fn->name = token(Token::LCID).string();
    fnType->name = fn->name;
    fn->ns = m_ns;

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
      auto assignment = AST::createAssignment(token().loc);

      bool isIdent = false;
      if (next(Token::LCID)) {
        m_blockStack.back()->stackSlots++;
        assignment->left = parseIdentifier();
        isIdent = true;
      } else {
        assignment->left = parsePattern();
      }

      match('=');
      assignment->value = parseExpr();
      let->assignments.push_back(assignment);

      if (isIdent) {
        auto ident = AST::asIdentifier(assignment->left);
        setType(ident->name, TypeChecker::typeof(assignment->value, m_environment, m_lexer));
        m_scope->set(ident->name, ident);
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
      kase->pattern = parsePattern();
      this->match(TUPLE_TOKEN('=', '>'));
      kase->body = parseExprOrBody();
      kase->body->env = m_environment;
      match->cases.push_back(kase);

      popScope();
    }

    return match;
  }

  AST::PatternPtr Parser::parsePattern() {
    auto pattern = AST::createPattern(token().loc);
    pattern->constructorName = token(Token::UCID).string();

    auto ctor = getType<TypeConstructor *>(pattern->constructorName);
    pattern->tag = ctor->tag;

    match('(');
    unsigned i = 0;
    while (!next(')')) {
      m_blockStack.back()->stackSlots++;

      auto ident = AST::asIdentifier(parseIdentifier());
      m_scope->set(ident->name, ident);
      setType(ident->name, ctor->type->types[i++]);
      pattern->values.push_back(ident);
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
      param->name = token(Token::LCID).string();
      param->index = i++;
      params.push_back(param);
      m_scope->set(param->name, param);

      match(':');

      auto type = parseType();
      types.push_back(type);
      setType(param->name, type);

      if (auto dti = dynamic_cast<DataTypeInstance *>(type)) {
        if (auto dt = dynamic_cast<EnumType *>(dti->dataType)) {
          for (unsigned i = 0; i < dti->types.size(); i++) {
            setType(dt->generics[i], dti->types[i]);
          }
        }
      }

      if (!skip(',')) break;
    }

    match(')');
  }

  void Parser::parseGenerics(std::vector<std::string> &generics) {
    if (!skip('<')) {
      return;
    }

    do {
      auto name = token(Token::LCID).string();
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
      callee = parseIdentifier();
    }
    return parseCall(callee);
  }

  AST::ConstructorPtr Parser::parseConstructor(std::string ucid) {
    auto ctor = AST::createConstructor(token().loc);
    ctor->name = ucid;

    auto ctorType = getType<TypeConstructor *>(ucid);
    assert(ctorType);

    ctor->tag = ctorType->tag;
    ctor->size = ctorType->size;

    match('(');
    while (!next(')')) {
      ctor->arguments.push_back(parseExpr());
      if (!skip(',')) break;
    }
    match(')');

    return ctor;
  }

  AST::NodePtr Parser::parseCall(AST::NodePtr callee) {
    auto loc = token().loc;
    if (!skip('(')) {
      return callee;
    }
    auto call = AST::createCall(loc);
    call->callee = callee;

    if (callee->type == AST::Type::Identifier) {
      auto calleeID = AST::asIdentifier(callee);
      auto fn = m_scope->get(calleeID->name);
      if (fn && fn->type == AST::Type::Function) {
        calleeID->ns = AST::asFunction(fn)->ns;
      }
    }

    while (!next(')')) {
      call->arguments.push_back(parseExpr());
      if (!skip(',')) break;
    }
    match(')');

    return parseCall(call);
  }

  // Base nodes

  AST::NodePtr Parser::parseIdentifier(std::string ns) {
    auto loc = token().loc;
    auto name = namespaced(ns, token(Token::LCID).string());
    auto var = m_scope->get(name);
    if (auto ident = dynamic_cast<AST::Identifier *>(var.get())) {
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
          ident->isCaptured = true;
          scope->isRequired = true;
          m_scope->capturesScope = true;
          goto ident;
        }
      }
    }
    if (var) return var;
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
      auto type = getType(token(Token::LCID).string());
      if (!skip('<')) {
        return type;
      }

      auto dti = new DataTypeInstance();
      dti->dataType = type;
      do {
        dti->types.push_back(parseType());
        if (!next('>')) break;
      } while(!next('>'));
      match('>');
      //assert(dti->types.size() == dti->dataType->generics.size());
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
    return (m_lexer.token().type == Token::LCID || m_lexer.token().type == Token::UCID) && m_lexer.token().string() == str;
  }

  inline bool Parser::skip(std::string str) {
    if (next(str)) {
      m_lexer.nextToken();
      return true;
    } else {
      return false;
    }
  }

  void Parser::match(std::string expected) {
    if (!skip(expected)) {
      std::cerr << "Invalid token found: expected `" << Lexer::tokenType(m_lexer.token()) << "` to be `" << expected << "`" << "\n";
      m_lexer.printSource();
      throw std::runtime_error("Parser error");
    }
  }

  inline bool Parser::next(Token::Type t) {
    return m_lexer.token().type == t;
  }
}
