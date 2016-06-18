#include <fstream>
#include <sstream>
#include <unordered_map>

#include "parser/ast.h"
#include "opcodes.h"

#pragma once

namespace ceos {

  class Generator {
    public:
      Generator(AST::ProgramPtr ast, bool isDebug) :
        m_ast(ast),
        m_isDebug(isDebug) {}

      std::stringstream &generate(void);

      static void disassemble(std::stringstream &);

    private:
      void generateNode(AST::NodePtr);
      void generateCall(AST::CallPtr);
      void generateNumber(AST::NumberPtr);
      void generateIdentifier(AST::IdentifierPtr);
      void generateString(AST::StringPtr);
      void generateList(AST::ListPtr);
      void generateFunctionParameter(AST::FunctionParameterPtr);
      void generateFunctionDefinition(AST::FunctionPtr);
      void generateFunctionSource(AST::FunctionPtr);
      void generateIf(AST::IfPtr);
      void generateProgram(AST::ProgramPtr);
      void generateBlock(AST::BlockPtr);
      void generateObjectTagTest(AST::ObjectTagTestPtr);
      void generateObjectLoad(AST::ObjectLoadPtr);
      void generateStackStore(AST::StackStorePtr);
      void generateStackLoad(AST::StackLoadPtr);
      void generateBinaryOperation(AST::BinaryOperationPtr);
      void generateUnaryOperation(AST::UnaryOperationPtr);
      void generateMatch(AST::MatchPtr);
      void generateLet(AST::LetPtr);

      void generateConstructor(AST::CallPtr);

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
      std::vector<AST::FunctionPtr> m_functions;
      bool m_isDebug;
  };

}
