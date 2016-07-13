#include <memory>

#include "ast.h"
#include "environment.h"
#include "scope.h"
#include "type.h"

#pragma once

extern "C" std::string ROOT_DIR;

namespace Verve {
  class Lexer;
  class Token;

  class Parser {
  public:

    Parser(Lexer &lexer, std::string dirname, std::string ns = "");
    AST::ProgramPtr parse(bool typecheck = true);

  private:

    AST::ProgramPtr parseImport();
    AST::NodePtr parseDecl();

    AST::ProgramPtr import(std::string path, std::vector<std::string>  imports, std::string ns, std::string dirname);

    AST::EnumTypePtr parseTypeDecl();
    AST::TypeConstructorPtr parseTypeConstructor();

    AST::InterfacePtr parseInterface();
    AST::ImplementationPtr parseImplementation();

    AST::PrototypePtr parseVirtual();
    AST::PrototypePtr parseExtern();
    AST::PrototypePtr parsePrototype();

    AST::FunctionPtr parseTypelessFunction();
    AST::FunctionPtr parseFunction();

    AST::IfPtr parseIf();
    AST::LetPtr parseLet();
    AST::MatchPtr parseMatch();
    AST::PatternPtr parsePattern();

    AST::BlockPtr parseExprOrBody();
    AST::BlockPtr parseBody(AST::BlockPtr block = nullptr);

    void parseFunctionParams(
        std::vector<AST::FunctionParameterPtr> &params,
        std::vector<AST::AbstractTypePtr> &types);

    void parseGenerics(std::vector<std::string> &generics);

    AST::ConstructorPtr parseConstructor(std::string ucid);
    AST::NodePtr parseIdentifierFunctionOrCall();
    AST::NodePtr parseCall(AST::NodePtr callee);

    AST::NodePtr parseExpr(int precedence = 0);
    AST::NodePtr parseFactor();

    // Base nodes

    AST::NodePtr parseIdentifier(std::string ns = "");
    AST::NumberPtr parseNumber();
    AST::NumberPtr parseFloat();
    AST::StringPtr parseString();
    AST::ListPtr parseList();
    AST::AbstractTypePtr parseType();

    // Var helpers

    void pushScope();
    void popScope();

    // Lexer aliases

    inline Token &token();
    inline Token &token(Token::Type t);

    inline bool next(int c);
    inline bool skip(int c);
    inline void match(int c);

    inline bool next(std::string str);
    inline bool skip(std::string str);
    inline void match(std::string str);

    inline bool next(Token::Type t);

    // Properties

    Lexer &m_lexer;
    ParseScopePtr m_scope;
    std::vector<AST::BlockPtr> m_blockStack;
    std::string m_dirname;
    AST::ProgramPtr m_ast;
    EnvPtr m_env;
    std::string m_ns;
  };

  __used static std::string namespaced(std::string ns, std::string name) {
    if (!ns.empty()) {
      return ns + "#" + name;
    } else {
      return name;
    }
  }
}
