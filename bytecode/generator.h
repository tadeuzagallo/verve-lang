#include <fstream>
#include <sstream>
#include <unordered_map>

#include "ast/nodes.h"
#include "ast/visitor.h"
#include "opcodes.h"

#pragma once

namespace Verve {

class Generator : public AST::Visitor {
public:
  static void generate(AST::NodePtr, bool shouldLink, std::stringstream *bytecode);

  void emitOpcode(Opcode::Type);
  void emitJmp(Opcode::Type, AST::BlockPtr &);
  void emitJmp(Opcode::Type, AST::BlockPtr &, bool);
  void write(int64_t);
  void write(const std::string &);
  unsigned uniqueString(std::string &);

private:
  Generator(bool shouldLink, std::stringstream *output) :
    m_shouldLink(shouldLink),
    m_output(output) {}

  void generateFunctionSource(AST::Function *fn);

  /** Visitors **/
  virtual void visitNumber(AST::Number *);
  virtual void visitCall(AST::Call *);
  virtual void visitIdentifier(AST::Identifier *);
  virtual void visitString(AST::String *);
  virtual void visitList(AST::List *);
  virtual void visitIf(AST::If *);
  virtual void visitProgram(AST::Program *);
  virtual void visitBlock(AST::Block *);
  virtual void visitBinaryOperation(AST::BinaryOperation *);
  virtual void visitUnaryOperation(AST::UnaryOperation *);
  virtual void visitMatch(AST::Match *);
  virtual void visitAssignment(AST::Assignment *);
  virtual void visitConstructor(AST::Constructor *);
  virtual void visitFunction(AST::Function *);

  bool m_shouldLink;
  std::stringstream *m_output;
  std::vector<std::string> m_strings;
  std::vector<AST::Function *> m_functions;
  std::unordered_map<std::string, unsigned> m_slots;

  unsigned lookupID = 1;
  unsigned stackSlot = 0;
  bool capturesScope = true;
};

}
