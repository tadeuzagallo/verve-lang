#include <memory>

#include "ast.h"
#include "environment.h"
#include "scope.h"
#include "type.h"

#pragma once

namespace ceos {
  class Lexer;
  class Token;

  class Parser {
  public:

    Parser(Lexer &lexer);
    AST::ProgramPtr parse();

  private:

    AST::NodePtr parseDecl();

    void parseTypeDecl();
    void parseTypeConstructor(unsigned tag, EnumType *owner);

    AST::BlockPtr parseInterface();
    AST::BlockPtr parseImplementation();

    TypeFunction *parseVirtual(std::shared_ptr<Environment> declScope = nullptr);
    TypeFunction *parseExtern(std::shared_ptr<Environment> declScope = nullptr, std::string implementationSuffix = "");
    TypeFunction *parsePrototype(std::string implementationSuffix = "");

    AST::FunctionPtr parseTypelessFunction(std::string implementationName, std::shared_ptr<Environment> declScope = nullptr);
    AST::FunctionPtr parseFunction();

    AST::IfPtr parseIf();
    AST::BlockPtr parseLet();

    AST::BlockPtr parseExprOrBody();
    AST::BlockPtr parseBody(AST::BlockPtr block = nullptr);

    bool parseFunctionParams(
        std::vector<AST::FunctionParameterPtr> &params,
        std::vector<Type *> &types);

    void parseGenerics(std::vector<std::string> &generics);

    AST::NodePtr parseIdentifierFunctionOrCall();
    AST::NodePtr parseCall(AST::NodePtr callee);

    AST::NodePtr parseExpr(int precedence = 0);
    AST::NodePtr parseFactor();

    // Base nodes

    AST::NodePtr parseIdentifier(bool checkScope = false);
    AST::NumberPtr parseNumber();
    AST::StringPtr parseString();
    Type *parseType();

    // Type helpers

    void setType(std::string typeName, Type *type, std::shared_ptr<Environment> env = nullptr);
    template<typename T = Type *> T getType(std::string typeName);

    void pushTypeScope();
    void popTypeScope();

    // Var helpers

    void pushScope();
    void popScope();

    // Lexer aliases

    inline Token &token();
    inline Token &token(Token::Type t);

    inline bool next(char c);
    inline bool skip(char c);
    inline void match(char c);

    inline bool next(std::string str);
    inline bool skip(std::string str);

    inline bool next(Token::Type t);

    // Properties

    Lexer &m_lexer;
    std::shared_ptr<Environment> m_environment;
    ParseScopePtr m_scope;
    std::vector<AST::BlockPtr> m_blockStack;
  };
}
