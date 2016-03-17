#include <fstream>
#include <sstream>

#include "ast.h"
#include "opcodes.h"

#ifndef CEOS_GENERATOR_H
#define CEOS_GENERATOR_H

namespace ceos {

  class Generator {
    public:
      Generator(std::shared_ptr<AST::Program> ast) :
        m_ast(ast) {}

      std::stringstream &generate(void);

      static void disassemble(std::stringstream &);

    private:
      void generateNode(std::shared_ptr<AST>);
      void generateCall(std::shared_ptr<AST::Call>);
      void generateNumber(std::shared_ptr<AST::Number>);
      void generateID(std::shared_ptr<AST::ID>);
      bool handleSpecialCall(std::shared_ptr<AST::Call> call);
      void generateFunction(std::shared_ptr<AST::Call>);
      void generateIf(std::shared_ptr<AST::Call>);
      void generateProgram(std::shared_ptr<AST::Program>);

      void emitOpcode(Opcode::Type);
      void write(int);
      void write(const std::string &);

      static void printOpcode(std::stringstream &, Opcode::Type);

      std::shared_ptr<AST::Program> m_ast;
      std::stringstream m_output;
  };

}

#endif
