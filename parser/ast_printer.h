#include "ast.h"

#include <string>

#include "ast_visitor.h"

namespace Verve {
namespace AST {

class Printer : Visitor {
public:
  static void dump(AST::NodePtr ast);
  void print(unsigned depth, const char *format, ...);

private:
  virtual void visitProgram(Program *);
  virtual void visitBlock(Block *);
  virtual void visitNumber(Number *);
  virtual void visitIdentifier(Identifier *);
  virtual void visitString(String *);
  virtual void visitFunctionParameter(FunctionParameter *);
  virtual void visitCall(Call *);
  virtual void visitIf(If *);
  virtual void visitBinaryOperation(BinaryOperation *);
  virtual void visitUnaryOperation(UnaryOperation *);
  virtual void visitList(List *);
  virtual void visitPattern(Pattern *);
  virtual void visitCase(Case *);
  virtual void visitMatch(Match *);
  virtual void visitAssignment(Assignment *);
  virtual void visitConstructor(Constructor *);
  virtual void visitLet(Let *);
  virtual void visitInterface(Interface *);
  virtual void visitImplementation(Implementation *);
  virtual void visitBasicType(BasicType *);
  virtual void visitFunctionType(FunctionType *);
  virtual void visitTypeConstructor(TypeConstructor *);
  virtual void visitDataType(DataType *);
  virtual void visitEnumType(EnumType *);
  virtual void visitPrototype(Prototype *);
  virtual void visitFunction(Function *);

  unsigned depth = 0;
  unsigned indentation = 2;
  bool inlineNext = false;
};

}
}
