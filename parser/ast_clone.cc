#include "ast.h"

namespace Verve {
namespace AST {

NodePtr Node::clone() const {
  throw std::runtime_error("Trying to copy AST for virtual node");
}

NodePtr Program::clone() const {
  throw std::runtime_error("Can't copy whole program");
}

NodePtr Block::clone() const {
  auto block = std::make_shared<Block>(*this);
  for (auto &node : block->nodes) {
    node = node->clone();
  }
  return block;
}

NodePtr Call::clone() const {
  auto call = std::make_shared<Call>(*this);
  for (auto &argument : call->arguments) {
    argument = argument->clone();
  }
  call->callee = call->callee->clone();
  return call;
}

NodePtr If::clone() const {
  auto iff = std::make_shared<If>(*this);
  iff->condition = iff->condition->clone();
  iff->ifBody = asBlock(iff->ifBody->clone());
  if (iff->elseBody) {
    iff->elseBody = asBlock(iff->elseBody->clone());
  }
  return iff;
}

NodePtr BinaryOperation::clone() const {
  auto binop = std::make_shared<BinaryOperation>(*this);
  binop->lhs = binop->lhs->clone();
  binop->rhs = binop->rhs->clone();
  return binop;
}

NodePtr UnaryOperation::clone() const {
  auto unop = std::make_shared<UnaryOperation>(*this);
  unop->operand = unop->operand->clone();
  return unop;
}

NodePtr List::clone() const {
  auto lst = std::make_shared<List>(*this);
  for (auto &item : lst->items) {
    item = item->clone();
  }
  return lst;
}

NodePtr Pattern::clone() const {
  auto pattern = std::make_shared<Pattern>(*this);
  for (auto &value : pattern->values) {
    value = asIdentifier(value->clone());
  }
  // value is copied on Match::copy and Assignment::copy
  return pattern;
}

NodePtr Case::clone() const {
  auto kase = std::make_shared<Case>(*this);
  kase->pattern = asPattern(kase->pattern->clone());
  kase->body = asBlock(kase->body->clone());
  return kase;
}

NodePtr Match::clone() const {
  auto match = std::make_shared<Match>(*this);
  match->value = match->value->clone();
  for (auto &kase : match->cases) {
    kase = asCase(kase->clone());
    kase->pattern->value = match->value;
  }
  return match;
}

NodePtr Assignment::clone() const {
  auto assignment = createAssignment(m_loc);
  assignment->kind = kind;
  assignment->value = assignment->value->clone();
  if (kind == Assignment::Pattern) {
    assignment->left.pattern = asPattern(assignment->left.pattern->clone());
    assignment->left.pattern->value = assignment->value;
  } else if (kind == Assignment::Identifier) {
    assignment->left.ident = asIdentifier(assignment->left.ident->clone());
  } else {
    assert(false);
  }
  return assignment;
}

NodePtr Let::clone() const {
  auto let = std::make_shared<Let>(*this);
  for (auto &assignment : let->assignments) {
    assignment = asAssignment(assignment->clone());
  }
  let->block = asBlock(let->block->clone());
  return let;
}

NodePtr Constructor::clone() const {
  auto ctor = std::make_shared<Constructor>(*this);
  for (auto &argument : ctor->arguments) {
    argument = argument->clone();
  }
  return ctor;
}

NodePtr Interface::clone() const {
  auto interface = std::make_shared<Interface>(*this);
  for (auto &fn : interface->functions) {
    fn = std::dynamic_pointer_cast<FunctionInterface>(fn->clone());
  }
  return interface;
}

NodePtr Implementation::clone() const {
  auto implementation = std::make_shared<Implementation>(*this);
  implementation->type = asAbstractType(implementation->type->clone());
  for (auto &fn : implementation->functions) {
    fn = std::dynamic_pointer_cast<FunctionInterface>(fn->clone());
  }
  return implementation;
}

NodePtr FunctionType::clone() const {
  auto fnType = std::make_shared<FunctionType>(*this);
  for (auto &param : fnType->params) {
    param = asAbstractType(param->clone());
  }
  fnType->returnType = asAbstractType(fnType->returnType->clone());
  return fnType;
}

NodePtr DataType::clone() const {
  auto dataType = std::make_shared<DataType>(*this);
  for (auto &param : dataType->params) {
    param = asAbstractType(param->clone());
  }
  return dataType;
}

NodePtr EnumType::clone() const {
  auto enumType = std::make_shared<EnumType>(*this);
  for (auto &constructor : enumType->constructors) {
    constructor = asTypeConstructor(constructor->clone());
  }
  return enumType;
}

NodePtr TypeConstructor::clone() const {
  auto typeCtor = std::make_shared<TypeConstructor>(*this);
  for (auto &type : typeCtor->types) {
    type = asAbstractType(type->clone());
  }
  return typeCtor;
}

NodePtr Prototype::clone() const {
  auto prototype = std::make_shared<Prototype>(*this);
  for (auto &param : prototype->params) {
    param = asAbstractType(param->clone());
  }
  prototype->returnType = asAbstractType(prototype->returnType->clone());
  return prototype;
}

NodePtr Function::clone() const {
  auto fn = std::make_shared<Function>(*this);
  if (fn->type) {
    fn->type = asPrototype(fn->type->clone());
  }
  for (auto &param : fn->parameters) {
    param = asFunctionParameter(param->clone());
  }
  fn->body = asBlock(fn->body->clone());
  return fn;
}

}
}
