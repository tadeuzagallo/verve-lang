#include "visitor.h"

namespace Verve {
namespace AST {

void Visitor::visitProgram(Program *program) {
  visitBlock(program);
}

void Visitor::visitBlock(Block *block) {
  for (const auto &node : block->nodes) {
    node->visit(this);
  }
}

void Visitor::visitCall(Call *call) {
  call->callee->visit(this);
  for (const auto &arg : call->arguments) {
    arg->visit(this);
  }
}

void Visitor::visitNumber(__unused Number *_) {
  // nothing to do
}

void Visitor::visitIdentifier(__unused Identifier *_) {
  // nothing to do
}

void Visitor::visitString(__unused String *_) {
  // nothing to do
}

void Visitor::visitFunction(Function *fn) {
  for (const auto &param : fn->parameters) {
    param->visit(this);
  }
  fn->body->visit(this);
}

void Visitor::visitFunctionParameter(__unused FunctionParameter *_) {
  // nothing to do
}

void Visitor::visitIf(If *iff) {
  iff->condition->visit(this);
  iff->ifBody->visit(this);
  if (iff->elseBody)
    iff->elseBody->visit(this);
}

void Visitor::visitBinaryOperation(BinaryOperation *binop) {
  binop->lhs->visit(this);
  binop->rhs->visit(this);
}

void Visitor::visitUnaryOperation(UnaryOperation *unop) {
  unop->operand->visit(this);
}

void Visitor::visitList(List *lst) {
  for (const auto &item : lst->items) {
    item->visit(this);
  }
}

void Visitor::visitMatch(Match *match) {
  match->value->visit(this);
  for (const auto &kase : match->cases) {
    kase->visit(this);
  }
}

void Visitor::visitCase(Case *kase) {
  kase->pattern->visit(this);
  kase->body->visit(this);
}

void Visitor::visitPattern(Pattern *pattern) {
  for (const auto &value : pattern->values) {
    value->visit(this);
  }
}

void Visitor::visitLet(Let *let) {
  for (const auto &assignment : let->assignments) {
    assignment->visit(this);
  }
  let->block->visit(this);
}

void Visitor::visitConstructor(Constructor *ctor) {
  for (const auto &arg : ctor->arguments) {
    arg->visit(this);
  }
}

void Visitor::visitAssignment(Assignment *assignment) {
  if (assignment->kind == Assignment::Identifier) {
    assignment->left.ident->visit(this);
  } else if (assignment->kind == Assignment::Pattern) {
    assignment->left.pattern->visit(this);
  } else {
    assert(false);
  }
  assignment->value->visit(this);
}

void Visitor::visitInterface(Interface *interface) {
  for (const auto &fn : interface->functions) {
    fn->visit(this);
  }
}

void Visitor::visitImplementation(Implementation *implementation) {
  implementation->type->visit(this);
  for (const auto &fn : implementation->functions) {
    fn->visit(this);
  }
}

void Visitor::visitBasicType(__unused BasicType *_) {
  // nothing to do
}

void Visitor::visitFunctionType(FunctionType *fnType) {
  for (const auto &param : fnType->params) {
    param->visit(this);
  }
  fnType->returnType->visit(this);
}

void Visitor::visitDataType(DataType *dataType) {
  for (const auto &param : dataType->params) {
    param->visit(this);
  }
}

void Visitor::visitEnumType(EnumType *enumType) {
  for (const auto &ctor : enumType->constructors) {
    ctor->visit(this);
  }
}

void Visitor::visitTypeConstructor(TypeConstructor *typeCtor) {
  for (const auto &type : typeCtor->types) {
    type->visit(this);
  }
}

void Visitor::visitPrototype(Prototype *prototype) {
  visitFunctionType(prototype);
}

}
}
