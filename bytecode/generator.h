#include <fstream>
#include <sstream>
#include <unordered_map>

#include "parser/ast.h"
#include "opcodes.h"

#pragma once

namespace Verve {

  struct Generator {
      Generator(AST::ProgramPtr ast, bool isDebug) :
        m_ast(ast),
        m_isDebug(isDebug) {}

      std::stringstream &generate(void);
      void generateFunctionSource(AST::Function *fn);

      static void disassemble(std::stringstream &);

      void emitOpcode(Opcode::Type);
      void emitJmp(Opcode::Type, AST::BlockPtr &);
      void emitJmp(Opcode::Type, AST::BlockPtr &, bool);
      void write(int64_t);
      void write(const std::string &);

      unsigned uniqueString(std::string &);

      static void printOpcode(std::stringstream &, Opcode::Type);

      AST::ProgramPtr  m_ast;
      std::stringstream m_output;
      std::vector<std::string> m_strings;
      std::vector<AST::Function *> m_functions;
      std::unordered_map<std::string, unsigned> m_slots;
      bool m_isDebug;

      unsigned lookupID = 1;
      unsigned stackSlot = 0;
      bool capturesScope = true;
  };

}
