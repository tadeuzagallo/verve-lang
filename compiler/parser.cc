#include "parser.h"

#include "lexer.h"
#include "token.h"

namespace ceos {

  static std::unordered_map<std::string, TypeChain *> *InterfaceTypeInfo;
  static std::unordered_map<std::string, Type *> *ImplementationTypes;
  static std::string ImplementationTypeName = "";

  std::shared_ptr<AST::Program> Parser::parse(void) {
    m_types["Int"] = new BasicType("Int");
    m_types["Char"] = new BasicType("Char");
    m_types["Float"] = new BasicType("Float");
    m_types["Void"] = new BasicType("Void");
    m_types["List"] = new DataType("List", 1);
    m_types["String"] = new DataTypeInstance((DataType *)m_types["List"], (::ceos::Type *[]){ m_types["Char"] });

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
      if (node) {
        block->nodes.push_back(node);
      }
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
      auto maybeElse = static_cast<Token::ID *>(m_lexer.token());
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
    auto number = static_cast<Token::Number *>(m_lexer.token(Token::Type::NUMBER));
    auto ast = std::make_shared<AST::Number>(number->value);
    ast->loc = number->loc;
    ast->typeInfo = m_types["Int"];
    return ast;
  }

  std::shared_ptr<AST> Parser::parseID() {
    auto id = *static_cast<Token::ID *>(m_lexer.token(Token::Type::ID));

    if (id.name == "if") {
      return parseIf();
    } else if (id.name == "interface") {
      parseInterface();
      return nullptr;
    } else if (id.name == "implementation") {
      return parseImplementation();
    }

    std::shared_ptr<AST> ast, ref;
    if ((ref = m_scope->get(id.name, false)) != nullptr && ref->type == AST::Type::FunctionArgument) {
      ast = ref;
    } else {
      auto uid = uniqueString(id.name);

      ast = std::make_shared<AST::ID>(m_ast->strings[uid], uid);
      ast->loc = id.loc;

      if ((ref = m_scope->get(id.name)) && !m_scope->isInCurrentScope(id.name)) {
        ast->typeInfo = ref->typeInfo;
        if (ref->type == AST::Type::FunctionArgument) {
          AST::asFunctionArgument(ref)->isCaptured = true;
          m_scope->scopeFor(id.name)->isRequired = true;
          m_scope->capturesScope = true;
        }
      } else {
        ast->typeInfo = m_typeInfo[id.name];
      }
    }

    while (true) {
      if (m_lexer.token()->type == Token::Type::TYPE) {
        TypeChain *typeInfo = parseTypeInfo();
        m_typeInfo[AST::asID(ast)->name] = typeInfo;
        return nullptr;
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

    if (ast->type == AST::Type::Call) {
      typeCheck(AST::asCall(ast));
    }

    return ast;
  }

  std::shared_ptr<AST::Function> Parser::parseFunction(std::shared_ptr<AST::Call> &&call) {
    assert(call->callee->type == AST::Type::ID);

    auto fn = std::make_shared<AST::Function>();
    fn->name = AST::asID(call->callee);
    fn->typeInfo = nullptr;
    if (InterfaceTypeInfo != nullptr) {
      fn->typeInfo = InterfaceTypeInfo->operator[](fn->name->name);
    }
    if (!(fn->typeInfo = fn->typeInfo ?: m_typeInfo[fn->name->name])) {
      fprintf(stderr, "Defining function `%s` that does not have type information\n", fn->name->name.c_str());
      throw std::runtime_error("Missing type infomation");
    }

    fn->name->name += ImplementationTypeName;
    fn->name->uid = uniqueString(fn->name->name);
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

      auto fnArg = std::make_shared<AST::FunctionArgument>(argName, i);
      fnArg->typeInfo = fn->getTypeInfo()->types[i++];
      GenericType *gt;
      if ((gt = dynamic_cast<GenericType *>(fnArg->typeInfo))) {
        if (!(fnArg->typeInfo = ImplementationTypes->operator[](gt->typeName))) {
          throw std::runtime_error("Unknown type");
        }
      }
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
    auto string = static_cast<Token::String *>(m_lexer.token(Token::Type::STRING));

    auto uid = uniqueString(string->value);
    auto ast =  std::make_shared<AST::String>(m_ast->strings[uid], uid);
    ast->loc = string->loc;
    ast->typeInfo = m_types["String"];
    return ast;
  }

  std::shared_ptr<AST::Call> Parser::parseCall(std::shared_ptr<AST> &&callee) {
    auto start = callee->loc.start;

    m_lexer.ensure(Token::Type::L_PAREN);

    auto call = std::make_shared<AST::Call>();
    call->callee = callee;

    while (m_lexer.token()->type != Token::Type::R_PAREN) {
      auto argument = parseFactor();
      call->arguments.push_back(argument);
      if (m_lexer.token()->type != Token::Type::R_PAREN) {
        m_lexer.ensure(Token::Type::COMMA);
      }
    }

    auto end = m_lexer.token(Token::Type::R_PAREN)->loc.end;
    call->loc = { start, end };

    TypeChain *typeInfo;
    if ((typeInfo = dynamic_cast<TypeChain *>(callee->typeInfo))) {
      call->typeInfo = typeInfo->returnType();
    }

    return call;
  }

  TypeChain *Parser::parseTypeInfo(std::string *genericName, bool skipTypeToken) {
    if (skipTypeToken) {
      m_lexer.ensure(Token::Type::TYPE);
    }

    TypeChain *typeInfo = new TypeChain();
    do {
      Type *type = nullptr;

      if (m_lexer.skip(Token::Type::L_PAREN)) {
        type = parseTypeInfo(genericName, false);
        m_lexer.ensure(Token::Type::R_PAREN);
      } else {
        auto typeString = AST::asID(parseID())->name;
        if (genericName != nullptr && typeString == *genericName) {
          type = new GenericType(typeString);
        } else {
          type = m_types[typeString];
        }
      }

      if (!type) {
        throw std::runtime_error("Undefined type");
      }
      typeInfo->types.push_back(type);
    } while (m_lexer.skip(Token::Type::ARROW));

    return typeInfo;
  }
  
  void Parser::parseInterface() {
    auto interface = new TypeInterface();
    interface->name = ((Token::ID *)m_lexer.token(Token::Type::ID))->name;

    m_lexer.ensure(Token::Type::L_ANGLE);
    interface->genericName = static_cast<Token::ID *>(m_lexer.token(Token::Type::ID))->name;
    m_lexer.ensure(Token::Type::R_ANGLE);

    m_lexer.ensure(Token::Type::L_BRACE);
    while (!m_lexer.skip(Token::Type::R_BRACE)) {
      auto fnName = ((Token::ID *)m_lexer.token(Token::Type::ID))->name;
      auto typeInfo = parseTypeInfo(&interface->genericName);
      m_types[fnName] = interface;
      interface->functions[fnName] = typeInfo;
      m_typeInfo[fnName] = typeInfo;
    }

    m_types[interface->name] = interface;
  }

  std::shared_ptr<AST> Parser::parseImplementation() {
    auto implementation = new TypeImplementation();
    auto name = static_cast<Token::ID *>(m_lexer.token(Token::Type::ID))->name;
    auto interface = dynamic_cast<TypeInterface *>(m_types[name]);
    if (!interface) {
      throw std::runtime_error("Cannot find interface for implementation");
    }

    m_lexer.ensure(Token::Type::L_ANGLE);
    Token::ID *type = static_cast<Token::ID *>(m_lexer.token(Token::Type::ID));
    implementation->type = m_types[type->name];
    m_lexer.ensure(Token::Type::R_ANGLE);

    std::unordered_map<std::string, Type *> t;
    t[interface->genericName] = implementation->type;

    InterfaceTypeInfo = &interface->functions;
    ImplementationTypes = &t;
    ImplementationTypeName = implementation->type->toString();

    m_lexer.ensure(Token::Type::L_BRACE);
    auto block = parseBlock(Token::Type::R_BRACE);
    m_lexer.ensure(Token::Type::R_BRACE);

    InterfaceTypeInfo = nullptr;
    ImplementationTypes = nullptr;
    ImplementationTypeName = "";

    implementation->interface = interface;
    interface->implementations[implementation->type] = implementation;

    block->needsScope = false;
    return block;
  }

  void Parser::typeCheck(std::shared_ptr<AST::Call> &&call) {
    if (call->callee->type == AST::Type::ID) {
      auto callee = AST::asID(call->callee);
      auto &calleeName = callee->name;
      auto typeInfo = m_typeInfo[calleeName];
      if (!typeInfo) {
        fprintf(stderr, "Missing type information for `%s`\n", calleeName.c_str());
        throw;
      }

      if (call->arguments.size() != typeInfo->types.size() - 1) {
        fprintf(stderr, "Invalid type");
        throw;
      }

      for (unsigned i = 0; i < typeInfo->types.size() - 1; i++) {
        Type* expected = typeInfo->types[i];
        Type* actual = call->arguments[i]->typeInfo;

        if (dynamic_cast<GenericType *>(expected)) {
          auto impl = dynamic_cast<TypeInterface *>(m_types[calleeName])->implementations[actual];
          if (!impl) {
            fprintf(stderr, "Couldn't find implementation for `%s` with signature `%s`", calleeName.c_str(), call->typeInfo->toString().c_str());
            throw;
          }

          callee->name += impl->type->toString();
          callee->uid = uniqueString(callee->name);
          expected = impl->type;
        }

        TypeInterface *interface;
        if ((interface = dynamic_cast<TypeInterface *>(expected))) {
          if (interface->implementations[actual] != nullptr) {
            continue;
          }
        }

        if (!actual->equals(expected)) {
          fprintf(stderr, "Expected `%s` but got `%s`\n", expected->toString().c_str(), actual->toString().c_str());
          throw;
        }
      }
    }
  }

  // Helpers
  //
  unsigned Parser::uniqueString(std::string &str) {
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

}
