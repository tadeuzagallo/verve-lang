#include <memory>

#include "ast.h"
#include "old_scope.h"
#include "type.h"

#ifndef CEOS_PARSER_H
#define CEOS_PARSER_H

namespace ceos {

  class Lexer;
  class AST;

  class Parser {
    public:
      Parser(Lexer &lexer) : m_lexer(lexer) {
        m_scope = std::make_shared<OldScope<std::shared_ptr<AST>>>();
      }

      std::shared_ptr<AST::Program> parse(void);

    private:
      std::shared_ptr<AST::Call> parseCall(std::shared_ptr<AST> &&callee);
      std::shared_ptr<AST::Number> parseNumber(void);
      std::shared_ptr<AST> parseID(void);
      std::shared_ptr<AST::Function> parseFunction(std::shared_ptr<AST::Call> &&);
      std::shared_ptr<AST::String> parseString(void);
      std::shared_ptr<AST> parseFactor(void);
      std::shared_ptr<AST> parseIf(void);
      std::shared_ptr<AST::Block> parseBlock(Token::Type delim);

      TypeChain *parseTypeInfo(std::string *genericName = nullptr, bool skipTypeToken = true);
      void parseInterface();
      std::shared_ptr<AST> parseImplementation();
      void typeCheck(std::shared_ptr<AST::Call> &&);

      // Helpers
      unsigned uniqueString(std::string &);

      Lexer &m_lexer;
      std::shared_ptr<AST::Program> m_ast;
      std::shared_ptr<OldScope<std::shared_ptr<AST>>> m_scope;
      std::unordered_map<std::string, Type *> m_types;
      std::unordered_map<std::string, TypeChain *> m_typeInfo;
  };
}

#endif
