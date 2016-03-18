#include <fstream>
#include <sstream>
#include <unordered_map>

#include "ast.h"
#include "opcodes.h"

#ifndef CEOS_GENERATOR_H
#define CEOS_GENERATOR_H

namespace ceos {

  class Generator {
    public:
      Generator(std::shared_ptr<AST::Program> ast) : m_ast(ast) {
        m_currentScope = new Scope(this);
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

      struct Scope {
        std::unordered_map<std::string, std::shared_ptr<AST>> variables;
        Scope *parent;

        Scope(Generator *g): m_generator(*g) {
          parent = m_generator.m_currentScope;
          m_generator.m_currentScope = this;
        }

        ~Scope() {
          m_generator.m_currentScope = parent;
        }

        std::shared_ptr<AST> get(std::string name) {
          std::shared_ptr<AST> v = nullptr;
          auto s = this;
          do {
            v = s->variables[name];
          } while (!v && (s = s->parent));
          return v;
        }

        private:
          Generator &m_generator;
      };

      Scope *m_currentScope;
  };

}

#endif
