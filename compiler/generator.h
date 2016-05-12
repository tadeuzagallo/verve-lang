#include <fstream>
#include <sstream>
#include <unordered_map>

#include "ast.h"
#include "opcodes.h"
#include "old_scope.h"

#ifndef CEOS_GENERATOR_H
#define CEOS_GENERATOR_H

namespace ceos {

  class Generator {
    public:
      Generator(std::shared_ptr<AST::Program> ast) : m_ast(ast) {
        m_scope = std::make_shared<OldScope<std::shared_ptr<AST>>>();
      }

      std::stringstream &generate(void);

      static void disassemble(std::stringstream &);

    private:
      void generateNode(std::shared_ptr<AST>);
      void generateCall(std::shared_ptr<AST::Call>);
      void generateNumber(std::shared_ptr<AST::Number>);
      void generateID(std::shared_ptr<AST::ID>);
      void generateString(std::shared_ptr<AST::String>);
      void generateFunctionArgument(std::shared_ptr<AST::FunctionArgument>);
      void generateFunctionDefinition(std::shared_ptr<AST::Function>);
      void generateFunctionSource(std::shared_ptr<AST::Function>);
      void generateIf(std::shared_ptr<AST::If>);
      void generateProgram(std::shared_ptr<AST::Program>);
      void generateBlock(std::shared_ptr<AST::Block>);

      void emitOpcode(Opcode::Type);
      void emitJmp(Opcode::Type, std::shared_ptr<AST::Block>&);
      void emitJmp(Opcode::Type, std::shared_ptr<AST::Block>&, bool);
      void write(int64_t);
      void write(const std::string &);

      static void printOpcode(std::stringstream &, Opcode::Type);

      std::shared_ptr<AST::Program> m_ast;
      std::shared_ptr<OldScope<std::shared_ptr<AST>>> m_scope;
      std::stringstream m_output;
  };

}

#endif
