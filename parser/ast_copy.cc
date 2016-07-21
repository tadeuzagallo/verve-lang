#include "ast.h"

namespace Verve {
namespace AST {

NodePtr Node::copy() const {
  throw std::runtime_error("Trying to copy AST for virtual node");
}

NodePtr Program::copy() const {
  throw std::runtime_error("Can't copy whole program");
}

NodePtr Block::copy() const {
  auto block = std::make_shared<Block>(*this);
  for (auto &node : block->nodes) {
    node = node->copy();
  }
  if (block->env) {
    block->env = block->env->create();
  }
  return block;
}

NodePtr Call::copy() const {
  auto call = std::make_shared<Call>(*this);
  for (auto &argument : call->arguments) {
    argument = argument->copy();
  }
  call->callee = call->callee->copy();
  return call;
}

NodePtr If::copy() const {
  auto iff = std::make_shared<If>(*this);
  iff->condition = iff->condition->copy();
  iff->ifBody = asBlock(iff->ifBody->copy());
  if (iff->elseBody) {
    iff->elseBody = asBlock(iff->elseBody->copy());
  }
  return iff;
}

NodePtr BinaryOperation::copy() const {
  auto binop = std::make_shared<BinaryOperation>(*this);
  binop->lhs = binop->lhs->copy();
  binop->rhs = binop->rhs->copy();
  return binop;
}

NodePtr UnaryOperation::copy() const {
  auto unop = std::make_shared<UnaryOperation>(*this);
  unop->operand = unop->operand->copy();
  return unop;
}

NodePtr List::copy() const {
  auto lst = std::make_shared<List>(*this);
  for (auto &item : lst->items) {
    item = item->copy();
  }
  return lst;
}

NodePtr Pattern::copy() const {
  auto pattern = std::make_shared<Pattern>(*this);
  for (auto &value : pattern->values) {
    value = asIdentifier(value->copy());
  }
  // value is copied on Match::copy and Assignment::copy
  return pattern;
}

NodePtr Case::copy() const {
  auto kase = std::make_shared<Case>(*this);
  kase->pattern = asPattern(kase->pattern->copy());
  kase->body = asBlock(kase->body->copy());
  return kase;
}

NodePtr Match::copy() const {
  auto match = std::make_shared<Match>(*this);
  match->value = match->value->copy();
  for (auto &kase : match->cases) {
    kase = asCase(kase->copy());
    kase->pattern->value = match->value;
  }
  return match;
}

NodePtr Assignment::copy() const {
  auto assignment = createAssignment(m_loc);
  assignment->kind = kind;
  assignment->value = assignment->value->copy();
  if (kind == Assignment::Pattern) {
    assignment->left.pattern = asPattern(assignment->left.pattern->copy());
    assignment->left.pattern->value = assignment->value;
  } else if (kind == Assignment::Identifier) {
    assignment->left.ident = asIdentifier(assignment->left.ident->copy());
  } else {
    assert(false);
  }
  return assignment;
}

NodePtr Let::copy() const {
  auto let = std::make_shared<Let>(*this);
  for (auto &assignment : let->assignments) {
    assignment = asAssignment(assignment->copy());
  }
  let->block = asBlock(let->block->copy());
  return let;
}

NodePtr Constructor::copy() const {
  auto ctor = std::make_shared<Constructor>(*this);
  for (auto &argument : ctor->arguments) {
    argument = argument->copy();
  }
  return ctor;
}

NodePtr Interface::copy() const {
  auto interface = std::make_shared<Interface>(*this);
  for (auto &fn : interface->functions) {
    fn = std::dynamic_pointer_cast<FunctionInterface>(fn->copy());
  }
  return interface;
}

NodePtr Implementation::copy() const {
  auto implementation = std::make_shared<Implementation>(*this);
  implementation->type = asAbstractType(implementation->type->copy());
  for (auto &fn : implementation->functions) {
    fn = std::dynamic_pointer_cast<FunctionInterface>(fn->copy());
  }
  implementation->env = implementation->env->create();
  return implementation;
}

NodePtr FunctionType::copy() const {
  auto fnType = std::make_shared<FunctionType>(*this);
  for (auto &param : fnType->params) {
    param = asAbstractType(param->copy());
  }
  fnType->returnType = asAbstractType(fnType->returnType->copy());
  return fnType;
}

NodePtr DataType::copy() const {
  auto dataType = std::make_shared<DataType>(*this);
  for (auto &param : dataType->params) {
    param = asAbstractType(param->copy());
  }
  return dataType;
}

NodePtr EnumType::copy() const {
  auto enumType = std::make_shared<EnumType>(*this);
  for (auto &constructor : enumType->constructors) {
    constructor = asTypeConstructor(constructor->copy());
  }
  return enumType;
}

NodePtr TypeConstructor::copy() const {
  auto typeCtor = std::make_shared<TypeConstructor>(*this);
  for (auto &type : typeCtor->types) {
    type = asAbstractType(type->copy());
  }
  return typeCtor;
}

NodePtr Prototype::copy() const {
  auto prototype = std::make_shared<Prototype>(*this);
  for (auto &param : prototype->params) {
    param = asAbstractType(param->copy());
  }
  prototype->returnType = asAbstractType(prototype->returnType->copy());
  return prototype;
}

NodePtr Function::copy() const {
  auto fn = std::make_shared<Function>(*this);
  if (fn->type) {
    fn->type = asPrototype(fn->type->copy());
  }
  for (auto &param : fn->parameters) {
    param = asFunctionParameter(param->copy());
  }
  fn->body = asBlock(fn->body->copy());
  return fn;
}

}
}
