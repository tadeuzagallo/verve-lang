#include "nodes.h"

#include "visitor.h"

namespace Verve {
namespace AST {

void Assignment::visit(Visitor *visitor) {
  visitor->visitAssignment(this);
}

void Identifier::visit(Visitor *visitor) {
  visitor->visitIdentifier(this);
}

void FunctionType::visit(Visitor *visitor) {
  visitor->visitFunctionType(this);
}

void UnaryOperation::visit(Visitor *visitor) {
  visitor->visitUnaryOperation(this);
}

void BinaryOperation::visit(Visitor *visitor) {
  visitor->visitBinaryOperation(this);
}

void TypeConstructor::visit(Visitor *visitor) {
  visitor->visitTypeConstructor(this);
}

void FunctionParameter::visit(Visitor *visitor) {
  visitor->visitFunctionParameter(this);
}

void If::visit(Visitor *visitor) {
  visitor->visitIf(this);
}

void Let::visit(Visitor *visitor) {
  visitor->visitLet(this);
}

void Call::visit(Visitor *visitor) {
  visitor->visitCall(this);
}

void Case::visit(Visitor *visitor) {
  visitor->visitCase(this);
}

void List::visit(Visitor *visitor) {
  visitor->visitList(this);
}

void Block::visit(Visitor *visitor) {
  visitor->visitBlock(this);
}

void Match::visit(Visitor *visitor) {
  visitor->visitMatch(this);
}

void Number::visit(Visitor *visitor) {
  visitor->visitNumber(this);
}

void String::visit(Visitor *visitor) {
  visitor->visitString(this);
}

void Pattern::visit(Visitor *visitor) {
  visitor->visitPattern(this);
}

void Program::visit(Visitor *visitor) {
  visitor->visitProgram(this);
}

void DataType::visit(Visitor *visitor) {
  visitor->visitDataType(this);
}

void EnumType::visit(Visitor *visitor) {
  visitor->visitEnumType(this);
}

void Function::visit(Visitor *visitor) {
  visitor->visitFunction(this);
}

void BasicType::visit(Visitor *visitor) {
  visitor->visitBasicType(this);
}

void Implementation::visit(Visitor *visitor) {
  visitor->visitImplementation(this);
}

void Interface::visit(Visitor *visitor) {
  visitor->visitInterface(this);
}

void Prototype::visit(Visitor *visitor) {
  visitor->visitPrototype(this);
}

void Constructor::visit(Visitor *visitor) {
  visitor->visitConstructor(this);
}

}
}
