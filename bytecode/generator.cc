#include "generator.h"
#include "opcodes.h"
#include "sections.h"

#include "parser/parser.h"

#include <algorithm>

namespace Verve {

std::stringstream Generator::generate(AST::NodePtr node, bool shouldLink) {
  Generator gen{shouldLink};
  node->visit(&gen);

  auto text = gen.m_output.str();
  gen.m_output.str(std::string());
  gen.m_output.clear();

  if (gen.m_functions.size()) {
    for (unsigned i = 0; i < gen.m_functions.size(); i++) {
      gen.write(Section::FunctionHeader);
      gen.generateFunctionSource(gen.m_functions[i]);
    }
  }

  auto functions = gen.m_output.str();
  gen.m_output.str(std::string());
  gen.m_output.clear();

  if (gen.m_strings.size()) {
    gen.write(Section::Header);
    gen.write(Section::Strings);

    for (const auto &str : gen.m_strings) {
      gen.write(str);
    }
  }

  unsigned index = gen.m_output.tellp();
  while (index++ % WORD_SIZE) {
    gen.m_output.put(1);
  }

  if (functions.length()) {
    gen.write(Section::Header);
    gen.write(Section::Functions);
    gen.m_output << functions;
  }

  gen.write(Section::Header);
  gen.write(Section::Text);
  gen.write(gen.lookupID);
  gen.m_output << text;

  gen.emitOpcode(Opcode::exit);
  gen.m_output.seekg(0);

  return std::move(gen.m_output);
}

void Generator::generateFunctionSource(AST::Function *fn) {
  std::string fnName = fn->name;
  if (fnName == "_") {
    static unsigned id = 0;
    fnName = "_" + std::to_string(id++);
  }
  write(uniqueString(fnName));
  write(fn->parameters.size());

  std::vector<unsigned> captured;
  for (unsigned i = 0; i < fn->parameters.size(); i++) {
    write(uniqueString(fn->parameters[i]->name));

    if (fn->parameters[i]->isCaptured) {
      captured.push_back(i);
    }
  }

  if (fn->needsScope) {
    emitOpcode(Opcode::create_lex_scope);
  }

  for (auto i : captured) {
    emitOpcode(Opcode::push_arg);
    write(i);
    emitOpcode(Opcode::put_to_scope);
    write(uniqueString(fn->parameters[i]->name));
  }

  m_slots.clear();
  stackSlot = 0;
  capturesScope = fn->body->env->capturesScope;
  fn->body->visit(this);

  if (fn->needsScope) {
    emitOpcode(Opcode::release_lex_scope);
  }

  emitOpcode(Opcode::ret);
}

void Generator::write(int64_t data) {
  m_output.write(reinterpret_cast<char *>(&data), sizeof(data));
}

void Generator::write(const std::string &data) {
  m_output << data;
  m_output.put(0);
}

void Generator::emitOpcode(Opcode::Type opcode) {
  if (m_shouldLink) {
    write(Opcode::address(opcode));
  } else {
    write(opcode);
  }
}

void Generator::emitJmp(Opcode::Type jmpType, AST::BlockPtr &body)  {
  emitJmp(jmpType, body, false);
}

void Generator::emitJmp(Opcode::Type jmpType, AST::BlockPtr &body, bool skipNextJump)  {
  emitOpcode(jmpType);
  unsigned beforePos = m_output.tellp();
  write(0); // placeholder

  body->visit(this);

  unsigned afterPos = m_output.tellp();
  m_output.seekp(beforePos);
  write((afterPos - beforePos)
      + (skipNextJump ? (3 * WORD_SIZE) : WORD_SIZE) // special case for if with else
  );
  m_output.seekp(afterPos);
}

unsigned Generator::uniqueString(std::string &str) {
  auto it = std::find(m_strings.begin(), m_strings.end(), str);
  if (it != m_strings.end()) {
    return it - m_strings.begin();
  } else {
    unsigned id = m_strings.size();
    m_strings.push_back(str);
    return id;
  }
}

/** Visitors **/

void Generator::visitNumber(AST::Number *number) {
  if (number->isFloat) {
    emitOpcode(Opcode::push);
    write(*(int64_t *)&number->value);
  } else {
    emitOpcode(Opcode::push);
    write(number->value);
  }
}

void Generator::visitCall(AST::Call *call) {
  for (unsigned i = call->arguments.size(); i > 0;) {
    call->arguments[--i]->visit(this);
  }

  call->callee->visit(this);

  emitOpcode(Opcode::call);
  write(call->arguments.size());
}


void Generator::visitIdentifier(AST::Identifier *ident) {
  if (ident->isFunctionParameter) {
    emitOpcode(Opcode::push_arg);
    write(ident->index);
    return;
  }

  if (!ident->isCaptured) {
    auto it = m_slots.find(ident->name);
    if (it != m_slots.end()) {
      emitOpcode(Opcode::stack_load);
      write(it->second);
      return;
    }
  }

  emitOpcode(Opcode::lookup);
  auto name = namespaced(ident->ns, ident->name);
  write(uniqueString(name));

  // temporarily disable lookup cache - logic is weak
  write(0);
  //if (capturesScope) {
    //write(0);
  //} else {
    //write(lookupID++);
  //}
}

void Generator::visitString(AST::String *str) {
  emitOpcode(Opcode::load_string);
  write(uniqueString(str->value));
}

void Generator::visitList(AST::List *lst) {
  emitOpcode(Opcode::alloc_list);
  write(lst->items.size() + 1);

  unsigned index = 1;
  for (const auto &item : lst->items) {
    item->visit(this);
    emitOpcode(Opcode::obj_store_at);
    write(index++);
  }
}

void Generator::visitIf(AST::If *iff) {
  iff->condition->visit(this);

  bool hasElse = iff->elseBody != nullptr && iff->elseBody->nodes.size() > 0;
  emitJmp(Opcode::jz, iff->ifBody, hasElse);

  if (hasElse) {
    emitJmp(Opcode::jmp, iff->elseBody);
  }
}

void Generator::visitProgram(AST::Program *program) {
  for (const auto &import : program->imports) {
    import->visit(this);
  }

  visitBlock(program);
}

void Generator::visitBlock(AST::Block *block) {
  if (block->stackSlots > 0) {
    emitOpcode(Opcode::stack_alloc);
    write(block->stackSlots * WORD_SIZE);
  }

  for (const auto &node : block->nodes) {
    node->visit(this);
  }

  if (block->stackSlots > 0) {
    emitOpcode(Opcode::stack_free);
    write(block->stackSlots * WORD_SIZE);
  }
}

void Generator::visitBinaryOperation(AST::BinaryOperation *binop) {
  binop->rhs->visit(this);
  binop->lhs->visit(this);

  auto opstr = std::string(reinterpret_cast<char *>(&binop->op));

  emitOpcode(Opcode::lookup);
  write(uniqueString(opstr));
  write(lookupID++);

  emitOpcode(Opcode::call);
  write(2);
}

void Generator::visitUnaryOperation(AST::UnaryOperation *unop) {
  unop->operand->visit(this);

  auto opstr = "unary_" + std::string(reinterpret_cast<char *>(&unop->op));

  emitOpcode(Opcode::lookup);
  write(uniqueString(opstr));
  write(lookupID++);

  emitOpcode(Opcode::call);
  write(1);
}

void Generator::visitMatch(AST::Match *match) {
  const auto size = match->cases.size();
  long long pos[size - 1];
  for (unsigned i = 0; i < size; i++) {
    const auto &kase = match->cases[i];

    match->value->visit(this);

    emitOpcode(Opcode::obj_load);
    write(-1);

    emitOpcode(Opcode::push);
    write(kase->pattern->tag);

    std::string fnName = std::string("==");
    emitOpcode(Opcode::lookup);
    write(uniqueString(fnName));
    write(lookupID++);
    emitOpcode(Opcode::call);
    write(2);

    emitOpcode(Opcode::jz);
    auto offset = m_output.tellp();
    write(0);
    for (unsigned j = 0; j < kase->pattern->values.size(); j++) {
      auto slot = stackSlot++;
      m_slots[kase->pattern->values[j]->name] = slot;
      match->value->visit(this);
      emitOpcode(Opcode::obj_load);
      write(j);
      emitOpcode(Opcode::stack_store);
      write(slot);
    }
    kase->body->visit(this);
    auto end = m_output.tellp();
    m_output.seekp(offset);

    unsigned extra = WORD_SIZE;
    auto jmp = i < size - 1;
    if (jmp) {
      extra += 2 * WORD_SIZE;
    }

    write((unsigned)(end - offset) + extra);
    m_output.seekp(end);

    if (jmp) {
      emitOpcode(Opcode::jmp);
      pos[i] = m_output.tellp();
      write(0);
    }
  }

  auto p = m_output.tellp();
  for (unsigned i = 0; i < size - 1; i++) {
    m_output.seekp(pos[i] );
    unsigned off = (long)p - pos[i];
    write(off + WORD_SIZE);
  }
  m_output.seekp(p);
}

static void handleCapture(AST::IdentifierPtr ident, unsigned stackSlot, Generator *gen) {
  if (ident->isCaptured) {
    gen->emitOpcode(Opcode::stack_load);
    gen->write(stackSlot);
    gen->emitOpcode(Opcode::put_to_scope);
    gen->write(gen->uniqueString(ident->name));
  }
}

void Generator::visitAssignment(AST::Assignment *assignment) {
  if (assignment->kind == AST::Assignment::Identifier) {
    auto slot = stackSlot++;
    assignment->value->visit(this);
    m_slots[assignment->left.ident->name] = slot;
    emitOpcode(Opcode::stack_store);
    write(slot);

    handleCapture(assignment->left.ident, slot, this);
  } else if (assignment->kind == AST::Assignment::Pattern) {
    assignment->value->visit(this);

    emitOpcode(Opcode::obj_tag_test);
    write(assignment->left.pattern->tag);

    for (unsigned i = 0; i < assignment->left.pattern->values.size(); i++) {
      assignment->value->visit(this);
      emitOpcode(Opcode::obj_load);
      write(i);

      auto slot = stackSlot++;
      auto ident = assignment->left.pattern->values[i];
      m_slots[ident->name] = slot;
      emitOpcode(Opcode::stack_store);
      write(slot);

      handleCapture(ident, slot, this);
    }
  } else {
    assert(false);
  }
}

void Generator::visitConstructor(AST::Constructor *ctor) {
  emitOpcode(Opcode::alloc_obj);
  write(ctor->size + 1); // args + tag
  write(ctor->tag); // tag

  for (unsigned i = 0; i < ctor->arguments.size(); i++) {
    ctor->arguments[i]->visit(this);
    emitOpcode(Opcode::obj_store_at);
    write(i + 1); // skip tag
  }
}

void Generator::visitFunction(AST::Function *fn) {
  if (dynamic_cast<TypeFunction *>(fn->body->env->get(fn->name).type)->usesInterface) {
    for (const auto &it : fn->instances) {
      it.second->visit(this);
    }
    return;
  }

  emitOpcode(Opcode::create_closure);
  write(m_functions.size());
  write(fn->body->env->capturesScope);
  if (fn->name != "_") {
    emitOpcode(Opcode::bind);
    auto name = namespaced(fn->ns, fn->name);
    write(uniqueString(name));
  }
  m_functions.push_back(fn);
}

}
