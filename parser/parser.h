#include <memory>

#include "ast.h"
#include "utils/old_scope.h"
#include "type.h"

#pragma once

namespace ceos {

  class Lexer;
  class AST;

  class Parser {
    public:
      Parser(Lexer &lexer) : m_lexer(lexer) {
        m_typeScope = m_scope = std::make_shared<OldScope<std::shared_ptr<AST>>>();
      }

      std::shared_ptr<AST::Program> parse(void);

    private:
      std::shared_ptr<AST::Call> parseCall(std::shared_ptr<AST> &&callee);
      std::shared_ptr<AST::Number> parseNumber(void);
      std::shared_ptr<AST> parseID();
      std::shared_ptr<AST::Function> parseFunction(std::shared_ptr<AST::Call> &&, TypeMap &);
      std::shared_ptr<AST::String> parseString(void);
      std::shared_ptr<AST> parseFactor();
      std::shared_ptr<AST> parseIf(void);
      std::shared_ptr<AST::Block> parseBlock(Token::Type delim);

      std::shared_ptr<AST::Block> parseInterface();
      std::shared_ptr<AST> parseImplementation();

      bool parseGenerics(TypeMap &);
      Type *parseType();
      TypeInfo *parsePrototype();

      void typeCheck(std::shared_ptr<AST::Call> &&);

      // Helpers
      unsigned uniqueString(std::string &);

      // Type helpers
      Type *getType(std::string);
      TypeInfo *getTypeInfo(std::string);

      void setType(std::string, Type *, bool parentScope = false);
      void setTypeInfo(std::string, TypeInfo *, bool parentScopee = false);

      void pushScope();
      void popScope();
      void pushTypeScope();
      void popTypeScope();

      Lexer &m_lexer;
      std::shared_ptr<AST::Program> m_ast;
      std::shared_ptr<OldScope<std::shared_ptr<AST>>> m_scope;
      std::shared_ptr<OldScope<std::shared_ptr<AST>>> m_typeScope;
  };
}
