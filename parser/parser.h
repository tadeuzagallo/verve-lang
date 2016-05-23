#include <memory>

#include "ast.h"
#include "utils/old_scope.h"
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

    AST::BlockPtr parseInterface();
    AST::BlockPtr parseImplementation();

    TypeFunction *parseVirtual();
    TypeFunction *parseExtern();
    TypeFunction *parsePrototype();

    AST::FunctionPtr parseTypelessFunction();
    AST::FunctionPtr parseFunction();

    AST::IfPtr parseIf();

    AST::BlockPtr parseFactorOrBody();
    AST::BlockPtr parseBody();

    bool parseFunctionParams(
        std::vector<AST::FunctionParameterPtr> &params,
        std::vector<Type *> &types);

    void parseGenerics(std::vector<std::string> &generics);

    AST::NodePtr parseIdentifierFunctionOrCall();
    AST::NodePtr parseCall(AST::NodePtr callee);

    AST::NodePtr parseFactor();

    // Base nodes

    AST::IdentifierPtr parseIdentifier(bool checkScope = false);
    AST::NumberPtr parseNumber();
    AST::StringPtr parseString();
    Type *parseType();

    // Type helpers

    struct Environment {
      std::unordered_map<std::string, Type *> types;
      std::shared_ptr<Environment> parent;
    };

    void setType(std::string &typeName, Type *type);
    template<typename T = Type *> T getType(std::string typeName);

    void pushTypeScope();
    void popTypeScope();

    // Var helpers

    struct Scope {
      std::unordered_map<std::string, AST::NodePtr> table;
      std::shared_ptr<Scope> parent;
    };

    void setVar(std::string &varName, AST::NodePtr var);
    AST::NodePtr getVar(std::string &varName);

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
    std::shared_ptr<Scope> m_scope;
  };
}
