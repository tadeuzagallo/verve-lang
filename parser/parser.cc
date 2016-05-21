#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {
  static Type *GenericInitialValue = (Type *)&GenericInitialValue;

  static std::string ImplementationTypeName = "";

  Parser::Parser(Lexer &lexer)
    : m_lexer(lexer)
  {
    m_scope = std::make_shared<OldScope<std::shared_ptr<AST>>>();
    m_environment = new Environment();

    setType("Int", new BasicType("Int"));
    setType("Char", new BasicType("Char"));
    setType("Float", new BasicType("Float"));
    setType("Void", new BasicType("Void"));
    setType("List", new DataType("List", 1));
    setType("String", new DataTypeInstance((DataType *)getType("List"), (::ceos::Type *[]){ getType("Char") }));

  }

  std::shared_ptr<AST::Program> Parser::parse(void) {
    m_ast = std::make_shared<AST::Program>();
    m_ast->loc.start = m_lexer.token().loc.start;
    m_ast->body = parseBlock(Token::Type::END);
    m_ast->body->needsScope = false;
    return m_ast;
  }

  std::shared_ptr<AST::Block> Parser::parseBlock(Token::Type delim) {
    auto block = std::make_shared<AST::Block>();
    while (m_lexer.token().type != delim) {
      std::shared_ptr<AST> node = parseFactor();
      if (node) {
        block->nodes.push_back(node);
      }
    }
    block->needsScope = m_scope->isRequired;
    block->capturesScope = m_scope->capturesScope;

    return block;
  }

  std::shared_ptr<AST> Parser::parseFactor(bool isCall) {
    switch (m_lexer.token().type) {
      case Token::Type::NUMBER:
        return parseNumber();
      case Token::Type::ID:
        return parseID(isCall);
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

    if (m_lexer.token().type == Token::Type::L_BRACE) {
      m_lexer.ensure(Token::Type::L_BRACE);
      _if->ifBody = parseBlock(Token::Type::R_BRACE);
      m_lexer.ensure(Token::Type::R_BRACE);
    } else {
      auto ifBody = std::make_shared<AST::Block>();
      ifBody->nodes.push_back(parseFactor());
      _if->ifBody = ifBody;
    }

    if (m_lexer.token().type == Token::Type::ID) {
      auto maybeElse = m_lexer.token().string();
      if (maybeElse == "else") {
        m_lexer.ensure(Token::Type::ID);

        if (m_lexer.token().type == Token::Type::L_BRACE) {
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
    auto &number = m_lexer.token(Token::Type::NUMBER);
    auto ast = std::make_shared<AST::Number>(number.number());
    ast->loc = number.loc;
    ast->typeInfo = getType("Int");
    return ast;
  }

  std::shared_ptr<AST> Parser::parseID(bool isCall) {
    auto loc = m_lexer.token().loc;
    auto ident = m_lexer.token(Token::Type::ID).string();

    if (ident == "if") {
      return parseIf();
    } else if (ident == "interface") {
      parseInterface();
      return nullptr;
    } else if (ident == "implementation") {
      return parseImplementation();
    } else if (ident == "extern") {
      auto name = m_lexer.token(Token::Type::ID).string();
      auto typeInfo = parsePrototype();
      typeInfo->external = true;
      setType(name, typeInfo);
      return nullptr;
    } else if (ident == "virtual") {
      throw std::runtime_error("Virtual functions can only be placed inside interfaces");
    }

    std::shared_ptr<AST> ast, ref;
    if ((ref = m_scope->get(ident, false)) != nullptr && ref->type == AST::Type::FunctionArgument) {
      ast = ref;
    } else {
      auto uid = uniqueString(ident);

      ast = std::make_shared<AST::ID>(m_ast->strings[uid], uid);
      ast->loc = loc;

      if ((ref = m_scope->get(ident)) && !m_scope->isInCurrentScope(ident)) {
        ast->typeInfo = ref->typeInfo;
        if (ref->type == AST::Type::FunctionArgument) {
          AST::asFunctionArgument(ref)->isCaptured = true;
          m_scope->scopeFor(ident)->isRequired = true;
          m_scope->capturesScope = true;
        }
      } else {
        ast->typeInfo = getType<TypeInfo *>(ident);
      }
    }

    pushTypeScope();

    int state = 0;
    static const int s_generics = 1;
    static const int s_call = 2;
    static const int s_scope = 4;
    static const int s_type = 8;

    TypeMap generics;
    if (parseGenerics(generics)) {
      state |= s_generics;
    }

    while (true) {
      if (m_lexer.skip(Token::Type::COLON)) {
        state |= s_type;

        ast->hasTypeAnnotation = true;
        ast->typeInfo = parseType();
        continue;
      } 

      if (m_lexer.token().type == Token::Type::L_PAREN) {
        state |= s_call;

        ast = parseCall(std::move(ast));
        continue;
      } 

      if (m_lexer.token().type == Token::Type::L_BRACE) {
        if (!(state & s_call)) {
          m_lexer.error(ast->loc, "Cannot declare method without arguments/parenthesis");
        }
        if (state & s_scope) {
          m_lexer.error(ast->loc, "Cannot declare function with two bodies");
        }
        state = s_scope;

        auto call = AST::asCall(ast);
        ast = parseFunction(std::move(call), generics);

        popTypeScope();

        continue;
      } else if (state & s_call) {
        if (state & (s_type | s_generics)) {
          m_lexer.error(ast->loc, "Extraneous %s in function call", (state & s_type) ? "return type" : "generic type");
        }

        state &= ~(s_type | s_generics | s_call);

        unsigned i = 0;
        for (auto arg : AST::asCall(ast)->arguments) {
          i++;
          if(arg->hasTypeAnnotation) {
            m_lexer.error(ast->loc, "Extraneous type annotation for arg #%d", i);
          }
        }

        typeCheck(AST::asCall(ast));
        continue;
      }

      break;
    }

    if (state & s_generics) {
      m_lexer.error(ast->loc, "Trailing generics");
    }

    if (!isCall && (state & s_type)) {
      m_lexer.error(ast->loc, "Trailing type annotation");
    }

    if (!(state & s_scope)) {
      popTypeScope();
    }

    return ast;
  }

  std::shared_ptr<AST::Function> Parser::parseFunction(std::shared_ptr<AST::Call> &&call, TypeMap &generics) {
    assert(call->callee->type == AST::Type::ID);

    auto fn = std::make_shared<AST::Function>();
    fn->name = AST::asID(call->callee);

    bool isImplementation = false;
    TypeInfo *ti;
    if ((ti = getType<TypeInfo *>(fn->name->name))) {
      isImplementation = true;
    }

    fn->name->name += ImplementationTypeName;
    fn->name->uid = uniqueString(fn->name->name);
    m_scope->set(fn->name->name, fn);
    m_scope->isRequired = true;

    auto typeInfo = isImplementation ? ti : new TypeInfo();
    fn->typeInfo = typeInfo;

    pushScope();

    unsigned i = 0;
    for (auto arg : call->arguments) {
      std::string argName;
      if (arg->type == AST::Type::ID) {
        argName = AST::asID(arg)->name;
      } else if (arg->type == AST::Type::FunctionArgument) {
        argName = AST::asFunctionArgument(arg)->name;
      } else {
        fprintf(stderr, "Can't handle argument type on function declaration");
        throw;
      }

      auto fnArg = std::make_shared<AST::FunctionArgument>(argName, i++);
      fn->arguments.push_back(fnArg);
      m_scope->set(argName, fnArg);

      if (isImplementation) {
        fnArg->typeInfo = arg->typeInfo = ti->types[i - 1];
      } else {
        fnArg->typeInfo = arg->typeInfo;
      }

      GenericType *gt;
      if ((gt = dynamic_cast<GenericType *>(fnArg->typeInfo))) {
        if (generics[gt->typeName] || (isImplementation && ti->generics[gt->typeName])) {
          typeInfo->generics[gt->typeName] = GenericInitialValue;
        } else {
          fnArg->typeInfo = getType(gt->typeName);
        }
      }

      if (!fnArg->typeInfo) {
        fprintf(stderr, "Defining function `%s` that is missing type information for arg #%d\n", fn->name->name.c_str(), i);
        throw;
      }

      if (!isImplementation) {
        typeInfo->types.push_back(fnArg->typeInfo);
      }
    }

    if (!isImplementation) {
      typeInfo->types.push_back(call->typeInfo);
    }

    // FIXME: hack for recursive functions
    setType(fn->name->name, typeInfo, true);

    m_lexer.ensure(Token::Type::L_BRACE);
    fn->body = parseBlock(Token::Type::R_BRACE);
    fn->loc.start = fn->name->loc.start;
    fn->loc.end = m_lexer.token(Token::Type::R_BRACE).loc.end;

    popScope();
    setType(fn->name->name, typeInfo, true);


    return fn;
  }

std::shared_ptr<AST::String> Parser::parseString() {
  auto string = std::move(m_lexer.token(Token::Type::STRING));

  auto uid = uniqueString(string.string());
  auto ast =  std::make_shared<AST::String>(m_ast->strings[uid], uid);
  ast->loc = string.loc;
  ast->typeInfo = getType("String");
  return ast;
}

std::shared_ptr<AST::Call> Parser::parseCall(std::shared_ptr<AST> &&callee) {
  auto start = callee->loc.start;

  m_lexer.ensure(Token::Type::L_PAREN);

  auto call = std::make_shared<AST::Call>();
  call->callee = callee;

  while (m_lexer.token().type != Token::Type::R_PAREN) {
    auto argument = parseFactor(true);
    call->arguments.push_back(argument);
    if (m_lexer.token().type != Token::Type::R_PAREN) {
      m_lexer.ensure(Token::Type::COMMA);
    }
  }

  auto end = m_lexer.token(Token::Type::R_PAREN).loc.end;
  call->loc = { start, end };

  TypeInfo *typeInfo;
  if ((typeInfo = dynamic_cast<TypeInfo *>(callee->typeInfo))) {
    call->typeInfo = typeInfo->returnType();
  }

  return call;
}

Type *Parser::parseType() {
  if (m_lexer.skip(Token::Type::L_PAREN)) {
    TypeInfo *typeInfo = new TypeInfo();
    while (!m_lexer.skip(Token::Type::R_PAREN)) {
      typeInfo->types.push_back(parseType());
      if (m_lexer.token().type != Token::Type::R_PAREN) {
        m_lexer.ensure(Token::Type::COMMA);
      }
    }
    m_lexer.ensure(Token::Type::COLON);
    typeInfo->types.push_back(parseType());
    return typeInfo;
  } else {
    auto typeString = m_lexer.token(Token::Type::ID).string();
    return getType(typeString);
  }
}

std::shared_ptr<AST::Block> Parser::parseInterface() {
  auto interface = new TypeInterface();
  interface->name = m_lexer.token(Token::Type::ID).string();

  m_lexer.ensure(Token::Type::L_ANGLE);
  interface->genericName = m_lexer.token(Token::Type::ID).string();
  m_lexer.ensure(Token::Type::R_ANGLE);

  setType(interface->name, interface);

  pushTypeScope();

  setType(interface->genericName, new GenericType(interface->genericName));

  m_lexer.ensure(Token::Type::L_BRACE);

  auto block = std::make_shared<AST::Block>();
  while (!m_lexer.skip(Token::Type::R_BRACE)) {
    auto next = m_lexer.token().string();
    if (next != "virtual") {
      auto node = parseID();
      if (node) {
        block->nodes.push_back(node);
      }
    } else {
      m_lexer.ensure(Token::Type::ID);

      auto name = m_lexer.token(Token::Type::ID).string();
      auto typeInfo = parsePrototype();

      typeInfo->interface = interface;
      interface->functions[name] = typeInfo;
      setType(name, typeInfo, true);
    }
  }
  popTypeScope();

  return block;
}

std::shared_ptr<AST> Parser::parseImplementation() {
  auto implementation = new TypeImplementation();
  auto name = m_lexer.token(Token::Type::ID).string();
  auto interface = dynamic_cast<TypeInterface *>(getType(name));
  if (!interface) {
    throw std::runtime_error("Cannot find interface for implementation");
  }

  m_lexer.ensure(Token::Type::L_ANGLE);
  auto typeName = m_lexer.token(Token::Type::ID).string();
  implementation->type = getType(typeName);
  if (!implementation->type) {
    throw std::runtime_error("Cannot implement interface `" + interface->name + "` for undefined type `" + typeName + "`");
  }
  m_lexer.ensure(Token::Type::R_ANGLE);

  ImplementationTypeName = implementation->type->toString();

  pushTypeScope();
  setType(interface->genericName, implementation->type);

  m_lexer.ensure(Token::Type::L_BRACE);
  auto block = parseBlock(Token::Type::R_BRACE);
  m_lexer.ensure(Token::Type::R_BRACE);

  popTypeScope();

  ImplementationTypeName = "";

  implementation->interface = interface;
  interface->implementations[implementation->type] = implementation;

  block->needsScope = false;
  return block;
}

TypeInfo *Parser::parsePrototype() {
  auto typeInfo = new TypeInfo();

  pushTypeScope();

  parseGenerics(typeInfo->generics);

  m_lexer.ensure(Token::Type::L_PAREN);
  while (m_lexer.token().type != Token::Type::R_PAREN) {
    typeInfo->types.push_back(parseType());
    if (!m_lexer.skip(Token::Type::COMMA)) {
      break;
    }
  }
  m_lexer.ensure(Token::Type::R_PAREN);

  m_lexer.ensure(Token::Type::COLON);
  typeInfo->types.push_back(parseType());

  popTypeScope();

  return typeInfo;
}

bool Parser::parseGenerics(TypeMap &generics) {
  if (m_lexer.skip(Token::Type::L_ANGLE)) {
    do {
      auto typeName = m_lexer.token(Token::Type::ID).string();
      generics[typeName] = new GenericType(typeName);
      setType(typeName, generics[typeName]);
      if (m_lexer.skip(Token::Type::R_ANGLE)) {
        break;
      }
    } while(m_lexer.skip(Token::Type::COMMA));

    return true;
  }

  return false;
}

void Parser::typeCheck(std::shared_ptr<AST::Call> &&call) {
  if (call->callee->type == AST::Type::ID) {
    auto callee = AST::asID(call->callee);
    auto &calleeName = callee->name;
    auto typeInfo = getType<TypeInfo *>(calleeName);
    if (!typeInfo) {
      fprintf(stderr, "Missing type information for `%s`\n", calleeName.c_str());
      throw;
    }

    if (call->arguments.size() != typeInfo->types.size() - 1) {
      fprintf(stderr, "Number of arguments for calling `%s` \n", calleeName.c_str());
      throw;
    }

    TypeMap generics = typeInfo->generics;
    for (unsigned i = 0; i < typeInfo->types.size() - 1; i++) {
      Type* expected = typeInfo->types[i];
      Type* actual = call->arguments[i]->typeInfo;

      GenericType *gt;
      if ((gt = dynamic_cast<GenericType *>(expected))) {
        Type *t;
        if ((t = generics[gt->typeName])) {
          if (t == GenericInitialValue) {
            generics[gt->typeName] = actual;
            continue;
          } else {
            expected = t;
          }
        } else {
          auto impl = getType<TypeInterface *>(calleeName)->implementations[actual];
          if (!impl) {
            fprintf(stderr, "Couldn't find implementation for `%s` with signature `%s`", calleeName.c_str(), call->typeInfo->toString().c_str());
            throw;
          }

          callee->name += impl->type->toString();
          callee->uid = uniqueString(callee->name);
          expected = impl->type;
        }
      }

      TypeInterface *interface;
      if ((interface = dynamic_cast<TypeInterface *>(expected))) {
        if (interface->implementations[actual] != nullptr) {
          continue;
        }
      }

      if (!actual->equals(expected)) {
        fprintf(stderr, "Expected `%s` but got `%s` on arg #%d for function `%s`\n", expected->toString().c_str(), actual->toString().c_str(), i+1, calleeName.c_str());
        throw;
      }
    }

    GenericType *gt;
    if ((gt = dynamic_cast<GenericType *>(call->typeInfo))) {
      Type *t;
      if ((t = generics[gt->typeName])) {
        if (t == GenericInitialValue) {
          throw std::runtime_error("Cannot resolve generic return type for function");
        } else {
          call->typeInfo = t;
        }
      }
    }

  }
}

// Helpers
unsigned Parser::uniqueString(std::string str) {
  unsigned uid;
  auto it = std::find(m_ast->strings.begin(), m_ast->strings.end(), str);
  if (it != m_ast->strings.end()) {
    uid = it - m_ast->strings.begin();
  } else {
    uid = str_uid++;
    m_ast->strings.push_back(str);
  }
  return uid;
}

// Type helpers
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

void Parser::setType(std::string typeName, Type *type, bool parentScope) {
  (parentScope ? m_environment->parent : m_environment)->types[typeName] = type;
}

void Parser::pushScope() {
  m_scope = m_scope->create();
  pushTypeScope();
}
void Parser::popScope() {
  m_scope = m_scope->restore();
  popTypeScope();
}

void Parser::pushTypeScope() {
  auto env = new Environment();
  env->parent = m_environment;
  m_environment = env;
}

void Parser::popTypeScope() {
  auto env = m_environment;
  m_environment = m_environment->parent;
  delete env;
}
}
