#include "ast.h"

namespace Verve {
namespace AST {

class Visitor {
public:
  virtual void visitProgram(Program *);
  virtual void visitBlock(Block *);
  virtual void visitCall(Call *);
  virtual void visitNumber(Number *);
  virtual void visitIdentifier(Identifier *);
  virtual void visitString(String *);
  virtual void visitFunction(Function *);
  virtual void visitFunctionParameter(FunctionParameter *);
  virtual void visitIf(If *);
  virtual void visitBinaryOperation(BinaryOperation *);
  virtual void visitUnaryOperation(UnaryOperation *);
  virtual void visitList(List *);
  virtual void visitMatch(Match *);
  virtual void visitCase(Case *);
  virtual void visitPattern(Pattern *);
  virtual void visitLet(Let *);
  virtual void visitConstructor(Constructor *);
  virtual void visitAssignment(Assignment *);
  virtual void visitInterface(Interface *);
  virtual void visitImplementation(Implementation *);
  virtual void visitBasicType(BasicType *);
  virtual void visitFunctionType(FunctionType *);
  virtual void visitDataType(DataType *);
  virtual void visitEnumType(EnumType *);
  virtual void visitTypeConstructor(TypeConstructor *);
  virtual void visitPrototype(Prototype *);
};

}
}
