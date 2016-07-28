#include "parser.h"

#include "lexer.h"
#include "naming.h"
#include "token.h"
#include "type_checker.h"

#include "utils/file.h"

#include <iostream>

std::string ROOT_DIR = "";

namespace Verve {

  static EnvPtr createEnv() {
    auto env = std::make_shared<Environment>();

    env->create("char").type = new BasicType("char");
    env->create("int").type = new BasicType("int");
    env->create("float").type = new BasicType("float");
    env->create("void").type = new BasicType("void");
    env->create("bool").type = new BasicType("bool");

    auto list = new EnumType();
    list->name = "list";
    list->generics.push_back("t");
    env->create("list").type = list;

    auto string = new DataTypeInstance();
    string->dataType = list;
    string->types.push_back(env->get("char").type);
    env->create("string").type = string;

    return env;
  }

  Parser::Parser(Lexer &lexer, std::string dirname, std::string ns) :
    m_lexer(lexer), m_dirname(dirname), m_ns(ns)
  {
    m_env = createEnv();
  }

  static auto isPrelude = false;
  AST::ProgramPtr Parser::parse() {
    auto program = AST::createProgram(Loc{0, 0});

    if (!isPrelude) {
      isPrelude = true;
      program->imports.push_back(import("runtime/prelude", {}, "", ROOT_DIR));
      isPrelude = false;
    }

    m_blockStack.push_back(program);
    while (skip("import")) {
      auto import = parseImport();
      if (import) {
        program->imports.push_back(import);
      }
    }
    while(!next(Token::END)) {
      auto node = parseDecl();
      if (node) {
        program->nodes.push_back(node);
      }
    }
    m_blockStack.pop_back();

    AST::Naming naming(m_env);
    program->visit(&naming);
    TypeChecker::check(program, m_env, m_lexer);
    m_ast = program;
    return program;
  }

  AST::ProgramPtr Parser::parseImport() {
    std::vector<std::string> imports;
    if (!skip('*')) {
      match('{');
      do {
        imports.push_back(token(Token::LCID).string());
        if (!skip(',')) break;
      } while (!next('}'));
      match('}');
    }

    match("from");

    auto path = token(Token::STRING).string();

    std::string ns = "";
    if (skip("as")) {
      ns = token(Token::UCID).string();
    }

    return import(path, imports, ns, m_dirname);
  }

  AST::ProgramPtr Parser::import(std::string path, std::vector<std::string>  imports, std::string ns, std::string dirname) {
    auto parser = parseFile(path, dirname, ns);

   if (imports.size() == 0) {
      for (auto it : parser.m_env->entries()) {
        m_env->create(namespaced(ns, it.first)) = it.second;
      }
    } else {
      for (auto import : imports) {
        m_env->create(namespaced(ns, import)) = parser.m_env->get(import);
      }
    }

    return parser.m_ast;
  }

  AST::NodePtr Parser::parseDecl() {
    if (next(Token::LCID)) {
      if (skip("interface")) {
        return parseInterface();
      } else if (skip("implementation")) {
        return parseImplementation();
      } else if (skip("type")) {
        return parseTypeDecl();
      } else if (skip("extern")) {
        return parseExtern();
      }
    }

    return parseExpr();
  }

  AST::EnumTypePtr Parser::parseTypeDecl() {
    auto type = AST::createEnumType(token().loc);
    type->name = token(Token::LCID).string();

    parseGenerics(type->generics);

    match('{');
    while (!skip('}')) {
      type->constructors.push_back(parseTypeConstructor());
    }

    return type;
  }

  AST::TypeConstructorPtr Parser::parseTypeConstructor() {
    auto ctor = AST::createTypeConstructor(token().loc);
    ctor->name = token(Token::UCID).string();

    match('(');
    while (!next(')')) {
      ctor->types.push_back(parseType());

      if (!skip(',')) {
        break;
      }
    }
    match(')');

    return ctor;
  }

  AST::InterfacePtr Parser::parseInterface() {
    auto interface = AST::createInterface(token().loc);
    interface->name = token(Token::LCID).string();

    match('<');
    interface->genericTypeName = token(Token::LCID).string();
    match('>');

    match('{');
    while (!skip('}')) {
      if (skip("virtual")) {
        auto virtualFunction = parseVirtual();
        interface->functions.push_back(virtualFunction);
        interface->virtualFunctions.push_back(virtualFunction->name);
      } else if (skip("fn")) {
        auto fn = parseFunction();
        interface->functions.push_back(fn);
        interface->concreteFunctions.push_back(fn->name);
      } else {
        match('}');
        break;
      }
    }

    return interface;
  }

  AST::ImplementationPtr Parser::parseImplementation() {
    auto implementation = AST::createImplementation(token().loc);
    implementation->interfaceName = token(Token::LCID).string();

    match('<');
    implementation->type = parseType();
    match('>');

    match('{');
    while(!skip('}')) {
      std::string name;

      if (skip("extern")) {
        auto fn = parseExtern();
        implementation->functions.push_back(fn);
      } else if (skip("fn")) {
        auto fn = parseTypelessFunction();
        implementation->functions.push_back(fn);
      } else {
        match('}');
        break;
      }
    }

    return implementation;
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
    } else if (next(Token::FLOAT)) {
      return parseFloat();
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

  AST::PrototypePtr Parser::parseVirtual() {
    auto prototype = parsePrototype();
    prototype->isVirtual = true;

    return prototype;
  }

  AST::PrototypePtr Parser::parseExtern() {
    auto prototype = parsePrototype();
    prototype->isExternal = true;

    return prototype;
  }

  AST::PrototypePtr Parser::parsePrototype() {
    auto prototype = AST::createPrototype(token().loc);
    prototype->name = token(Token::LCID).string();

    parseGenerics(prototype->generics);

    match('(');
    while (!next(')')) {
      prototype->params.push_back(parseType());
      if (!skip(',')) break;
    }
    match(')');

    match(TUPLE_TOKEN('-', '>'));
    prototype->returnType = parseType();

    return prototype;
  }

  AST::FunctionPtr Parser::parseTypelessFunction() {
    auto fn = AST::createFunction(token().loc);
    fn->name = token(Token::LCID).string();

    match('(');
    unsigned i = 0;
    while (!next(')')) {
      auto arg = AST::createFunctionParameter(token().loc);
      arg->name = token(Token::LCID).string();
      arg->index = i++;
      fn->parameters.push_back(arg);

      if (!skip(',')) break;
    }
    match(')');

    fn->body = AST::createBlock(token().loc);
    m_blockStack.push_back(fn->body);
    parseBody(fn->body);
    m_blockStack.pop_back();

    return fn;
  }

  AST::FunctionPtr Parser::parseFunction() {
    auto start = token().loc;

    auto fn = AST::createFunction(start);
    fn->type = AST::createPrototype(start);
    fn->type->name =
    fn->name = token(Token::LCID).string();
    fn->ns = m_ns;

    parseGenerics(fn->type->generics);

    parseFunctionParams(fn->parameters, fn->type->params);

    match(TUPLE_TOKEN('-', '>'));
    fn->type->returnType = parseType();

    fn->body = AST::createBlock(token().loc);
    m_blockStack.push_back(fn->body);
    parseBody(fn->body);
    m_blockStack.pop_back();

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

    let->block = AST::createBlock(token().loc);

    while (!next('{')) {
      auto assignment = AST::createAssignment(token().loc);

      if (next(Token::LCID)) {
        m_blockStack.back()->stackSlots++;
        assignment->left.ident = parseIdentifier();
        assignment->kind = AST::Assignment::Identifier;
      } else {
        assignment->left.pattern = parsePattern();
        assignment->kind = AST::Assignment::Pattern;
      }

      match('=');
      assignment->value = parseExpr();
      let->assignments.push_back(assignment);
    }

    parseBody(let->block);

    return let;
  }

  AST::MatchPtr Parser::parseMatch() {
    auto match = AST::createMatch(token().loc);
    match->value = parseExpr();

    this->match('{');
    while (!skip('}')) {
      auto kase = AST::createCase(token().loc);
      kase->pattern = parsePattern();
      kase->pattern->value = match->value;
      this->match(TUPLE_TOKEN('=', '>'));
      kase->body = parseExprOrBody();
      match->cases.push_back(kase);
    }

    return match;
  }

  AST::PatternPtr Parser::parsePattern() {
    auto pattern = AST::createPattern(token().loc);
    pattern->constructorName = token(Token::UCID).string();

    match('(');
    while (!next(')')) {
      m_blockStack.back()->stackSlots++;

      auto ident = AST::asIdentifier(parseIdentifier());
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
      std::vector<AST::AbstractTypePtr> &types)
  {
    match('(');

    unsigned i = 0;
    while (!next(')')) {
      auto param = AST::createFunctionParameter(token().loc);
      param->name = token(Token::LCID).string();
      param->index = i++;
      params.push_back(param);

      match(':');

      auto type = parseType();
      types.push_back(type);

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

    while (!next(')')) {
      call->arguments.push_back(parseExpr());
      if (!skip(',')) break;
    }
    match(')');

    return parseCall(call);
  }

  // Base nodes

  AST::IdentifierPtr Parser::parseIdentifier(std::string ns) {
    auto identifier = AST::createIdentifier(token().loc);
    identifier->name = namespaced(ns, token(Token::LCID).string());
    return identifier;
  }

  AST::NumberPtr Parser::parseNumber() {
    auto number = AST::createNumber(token().loc);
    number->value = token(Token::NUMBER).number();
    return number;
  }

  AST::NumberPtr Parser::parseFloat() {
    auto number = AST::createNumber(token().loc);
    number->value = token(Token::FLOAT).number();
    number->isFloat = true;
    return number;
  }

  AST::StringPtr Parser::parseString() {
    auto str = AST::createString(token().loc);
    str->value = token(Token::STRING).string();
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

  AST::AbstractTypePtr Parser::parseType() {
    if (skip('(')) {
      auto type = AST::createFunctionType(token().loc);
      while (!next(')')) {
        type->params.push_back(parseType());
        if (!skip(',')) break;
      }
      match(')');
      match(TUPLE_TOKEN('-', '>'));
      type->returnType = parseType();
      return type;
    } else {
      auto tkn = std::move(token(Token::LCID));

      if (!skip('<')) {
        auto type = AST::createBasicType(tkn.loc);
        type->name = tkn.string();
        return type;
      }

      auto dti = AST::createDataType(tkn.loc);
      dti->name = tkn.string();
      do {
        dti->params.push_back(parseType());
        if (!next('>')) break;
      } while(!next('>'));
      match('>');

      return dti;
    }
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
