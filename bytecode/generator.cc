#include "generator.h"
#include "opcodes.h"
#include "sections.h"

#include "parser/parser.h"

#include <algorithm>

namespace Verve {

  std::stringstream &Generator::generate() {
    m_ast->generateBytecode(this);

    auto text = m_output.str();
    m_output.str(std::string());
    m_output.clear();

    if (m_functions.size()) {
      for (unsigned i = 0; i < m_functions.size(); i++) {
        write(Section::FunctionHeader);
        generateFunctionSource(m_functions[i]);
      }
    }

    auto functions = m_output.str();
    m_output.str(std::string());
    m_output.clear();

    if (m_strings.size()) {
      write(Section::Header);
      write(Section::Strings);

      for (auto string : m_strings) {
        write(string);
      }
    }

    unsigned index = m_output.tellp();
    while (index++ % WORD_SIZE) {
      m_output.put(1);
    }

    if (functions.length()) {
      write(Section::Header);
      write(Section::Functions);
      m_output << functions;
    }

    write(Section::Header);
    write(Section::Text);
    write(lookupID);
    m_output << text;

    emitOpcode(Opcode::exit);
    m_output.seekg(0);

    return m_output;
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
    fn->body->generateBytecode(this);

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

    body->generateBytecode(this);

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

namespace AST {

void Number::generateBytecode(Generator *gen) {
  if (isFloat) {
    gen->emitOpcode(Opcode::push);
    gen->write(*(int64_t *)&value);
  } else {
    gen->emitOpcode(Opcode::push);
    gen->write(value);
  }
}

void Call::generateBytecode(Generator *gen) {
  for (unsigned i = arguments.size(); i > 0;) {
    arguments[--i]->generateBytecode(gen);
  }

  callee->generateBytecode(gen);

  gen->emitOpcode(Opcode::call);
  gen->write(arguments.size());
}


void Identifier::generateBytecode(Generator *gen) {
  if (isFunctionParameter) {
    gen->emitOpcode(Opcode::push_arg);
    gen->write(index);
    return;
  }

  if (!isCaptured) {
    auto it = gen->m_slots.find(name);
    if (it != gen->m_slots.end()) {
      gen->emitOpcode(Opcode::stack_load);
      gen->write(it->second);
      return;
    }
  }

  gen->emitOpcode(Opcode::lookup);
  auto name = namespaced(ns, this->name);
  gen->write(gen->uniqueString(name));
  gen->write(0);

  // temporarily disable lookup cache - logic is weak
  //if (gen->capturesScope) {
    //gen->write(0);
  //} else {
    //gen->write(gen->lookupID++);
  //}
}

void String::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::load_string);
  gen->write(gen->uniqueString(value));
}

void List::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::alloc_list);
  gen->write(items.size() + 1);

  unsigned index = 1;
  for (auto item : items) {
    item->generateBytecode(gen);
    gen->emitOpcode(Opcode::obj_store_at);
    gen->write(index++);
  }
}

void FunctionParameter::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::push_arg);
  gen->write(index);
}

void If::generateBytecode(Generator *gen) {
  condition->generateBytecode(gen);

  bool hasElse = elseBody != nullptr && elseBody->nodes.size() > 0;
  gen->emitJmp(Opcode::jz, ifBody, hasElse);

  if (hasElse) {
    gen->emitJmp(Opcode::jmp, elseBody);
  }
}

void Program::generateBytecode(Generator *gen) {
  for (const auto &import : imports) {
    import->generateBytecode(gen);
  }

  Block::generateBytecode(gen);
}

void Block::generateBytecode(Generator *gen) {
  if (stackSlots > 0) {
    gen->emitOpcode(Opcode::stack_alloc);
    gen->write(stackSlots * WORD_SIZE);
  }

  for (auto node : nodes) {
    node->generateBytecode(gen);
  }

  if (stackSlots > 0) {
    gen->emitOpcode(Opcode::stack_free);
    gen->write(stackSlots * WORD_SIZE);
  }
}

void BinaryOperation::generateBytecode(Generator *gen) {
  rhs->generateBytecode(gen);
  lhs->generateBytecode(gen);

  auto opstr = std::string(reinterpret_cast<char *>(&op));

  gen->emitOpcode(Opcode::lookup);
  gen->write(gen->uniqueString(opstr));
  gen->write(gen->lookupID++);

  gen->emitOpcode(Opcode::call);
  gen->write(2);
}

void UnaryOperation::generateBytecode(Generator *gen) {
  operand->generateBytecode(gen);

  auto opstr = "unary_" + std::string(reinterpret_cast<char *>(&op));

  gen->emitOpcode(Opcode::lookup);
  gen->write(gen->uniqueString(opstr));
  gen->write(gen->lookupID++);

  gen->emitOpcode(Opcode::call);
  gen->write(1);
}

void Match::generateBytecode(Generator *gen) {
  auto size = cases.size();
  long long pos[size - 1];
  for (unsigned i = 0; i < size; i++) {
    auto kase = cases[i];

    value->generateBytecode(gen);

    gen->emitOpcode(Opcode::obj_load);
    gen->write(-1);

    gen->emitOpcode(Opcode::push);
    gen->write(kase->pattern->tag);

    std::string fnName = std::string("==");
    gen->emitOpcode(Opcode::lookup);
    gen->write(gen->uniqueString(fnName));
    gen->write(gen->lookupID++);
    gen->emitOpcode(Opcode::call);
    gen->write(2);

    gen->emitOpcode(Opcode::jz);
    auto offset = gen->m_output.tellp();
    gen->write(0);
    for (unsigned j = 0; j < kase->pattern->values.size(); j++) {
      auto slot = gen->stackSlot++;
      gen->m_slots[kase->pattern->values[j]->name] = slot;
      value->generateBytecode(gen);
      gen->emitOpcode(Opcode::obj_load);
      gen->write(j);
      gen->emitOpcode(Opcode::stack_store);
      gen->write(slot);
    }
    kase->body->generateBytecode(gen);
    auto end = gen->m_output.tellp();
    gen->m_output.seekp(offset);

    unsigned extra = WORD_SIZE;
    auto jmp = i < size - 1;
    if (jmp) {
      extra += 2 * WORD_SIZE;
    }

    gen->write((unsigned)(end - offset) + extra);
    gen->m_output.seekp(end);

    if (jmp) {
      gen->emitOpcode(Opcode::jmp);
      pos[i] = gen->m_output.tellp();
      gen->write(0);
    }
  }

  auto p = gen->m_output.tellp();
  for (unsigned i = 0; i < size - 1; i++) {
    gen->m_output.seekp(pos[i] );
    unsigned off = (long)p - pos[i];
    gen->write(off + WORD_SIZE);
  }
  gen->m_output.seekp(p);
}

void Let::generateBytecode(Generator *gen) {
  for (auto assignment : assignments) {
    assignment->generateBytecode(gen);
  }

  block->generateBytecode(gen);
}

static void handleCapture(AST::IdentifierPtr ident, unsigned stackSlot, Generator *gen) {
  if (ident->isCaptured) {
    gen->emitOpcode(Opcode::stack_load);
    gen->write(stackSlot);
    gen->emitOpcode(Opcode::put_to_scope);
    gen->write(gen->uniqueString(ident->name));
  }
}

void Assignment::generateBytecode(Generator *gen) {
  switch (kind) {
    case Assignment::Identifier: {
      auto slot = gen->stackSlot++;
      value->generateBytecode(gen);
      gen->m_slots[left.ident->name] = slot;
      gen->emitOpcode(Opcode::stack_store);
      gen->write(slot);

      handleCapture(left.ident, slot, gen);
      break;
    }
    case Assignment::Pattern: {
      value->generateBytecode(gen);

      gen->emitOpcode(Opcode::obj_tag_test);
      gen->write(left.pattern->tag);

      for (unsigned i = 0; i < left.pattern->values.size(); i++) {
        value->generateBytecode(gen);
        gen->emitOpcode(Opcode::obj_load);
        gen->write(i);

        auto slot = gen->stackSlot++;
        auto ident = left.pattern->values[i];
        gen->m_slots[ident->name] = slot;
        gen->emitOpcode(Opcode::stack_store);
        gen->write(slot);

        handleCapture(ident, slot, gen);
      }
      break;
    }
    default:
      assert(false);
  }
}

void Constructor::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::alloc_obj);
  gen->write(size + 1); // args + tag
  gen->write(tag); // tag

  for (unsigned i = 0; i < arguments.size(); i++) {
    arguments[i]->generateBytecode(gen);
    gen->emitOpcode(Opcode::obj_store_at);
    gen->write(i + 1); // skip tag
  }
}

void Function::generateBytecode(Generator *gen) {
  if (dynamic_cast<TypeFunction *>(body->env->get(name).type)->usesInterface) {
    for (const auto &it : instances) {
      it.second->generateBytecode(gen);
    }
    return;
  }

  gen->emitOpcode(Opcode::create_closure);
  gen->write(gen->m_functions.size());
  gen->write(body->env->capturesScope);
  if (name != "_") {
    gen->emitOpcode(Opcode::bind);
    auto name = namespaced(ns, this->name);
    gen->write(gen->uniqueString(name));
  }
  gen->m_functions.push_back(this);
}

void Interface::generateBytecode(Generator *gen) {
  for (const auto &fn : functions) {
    fn->generateBytecode(gen);
  }
}

void Implementation::generateBytecode(Generator *gen) {
  for (const auto &fn : functions) {
    fn->generateBytecode(gen);
  }
}

void AbstractType::generateBytecode(__unused Generator *gen) {
  // Types never generate bytecode
}

}
}
