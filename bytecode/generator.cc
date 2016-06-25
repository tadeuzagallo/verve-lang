#include "generator.h"
#include "opcodes.h"
#include "sections.h"

#include "parser/parser.h"

#include <iostream>
#include <iomanip>

#define WORD_SIZE 8

namespace ceos {

  std::stringstream &Generator::generate() {
    m_ast->generateBytecode(this);

    auto text = m_output.str();
    m_output = std::stringstream();

    if (m_functions.size()) {
      for (unsigned i = 0; i < m_functions.size(); i++) {
        write(Section::FunctionHeader);
        generateFunctionSource(m_functions[i]);
      }
    }

    auto functions = m_output.str();
    m_output = std::stringstream();

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

    capturesScope = fn->capturesScope;
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
    if (m_isDebug) {
      write(opcode);
    } else {
      write(Opcode::opcodeAddress(opcode));
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

  static std::vector<std::string> strings;
  static std::vector<std::string> functions;
  static size_t width;
  static std::string padding("  ");
  void Generator::disassemble(std::stringstream &bytecode) {
    size_t size = bytecode.str().length();
    width = std::ceil(std::log10(size + 1)) + 1;

#define WRITE(OFFSET, ...) \
    std::cout << "[" \
    <<  std::setfill(' ') << std::setw(width) << ((int)bytecode.tellg() - ((OFFSET) * WORD_SIZE)) \
    << "] " << padding << __VA_ARGS__ << "\n"

read_section:
    READ_INT(bytecode, ceos);
    assert(ceos == 0xCE05);

    READ_INT(bytecode, section);

    switch (section) {
      case Section::Strings:
        padding = "";
        WRITE(2, "STRINGS:");
        padding = "  ";
        goto section_strings;
        break;
      case Section::Functions: {
        padding = "";
        WRITE(2, "FUNCTIONS:");
        padding = "  ";
        auto offset = bytecode.tellg();
        while (true) {
          READ_INT(bytecode, header);
          if (header == Section::FunctionHeader) {
            READ_INT(bytecode, fnID);
            functions.push_back(strings[fnID]);
            continue;
          } else if (header == Section::Header) {
            break;
          }
        }
        bytecode.seekg(offset);
        goto section_functions;
        break;
      }
      case Section::Text:
        padding = "";
        WRITE(2, "TEXT:");
        padding = "  ";
        goto section_code;
        break;
    }

section_strings:
    while (true) {
      READ_INT(bytecode, ceos);
      bytecode.seekg(-sizeof(ceos), bytecode.cur);
      if (ceos == Section::Header)  {
        goto read_section;
      }
      static unsigned str_index = 0;
      READ_STR(bytecode, str);
      while (bytecode.peek() == '\1') {
        bytecode.get();
      }
      strings.push_back(str);
      WRITE((float)(str.length() + 1)/WORD_SIZE, "$" << str_index++ << ": " << str);
    }

section_functions: {
      READ_INT(bytecode, fn_header);
      assert(fn_header == Section::FunctionHeader);

      READ_INT(bytecode, fn_id);
      READ_INT(bytecode, arg_count);

      std::stringstream args;
      for (int i = 0; i < arg_count; i++) {
        READ_INT(bytecode, argID);
        args << "$" << i << ": " << strings[argID];
        if (i < arg_count - 1) args << ", ";
      }

      padding = "";
      WRITE(3 + arg_count, strings[fn_id] << "(" << args.str() << "):");
      padding = "  ";

      while (true) {
        READ_INT(bytecode, header);
        bytecode.seekg(-sizeof(header), bytecode.cur);
        if (header == Section::FunctionHeader) {
          goto section_functions;
        } else if (header == Section::Header) {
          goto read_section;
        }

        READ_INT(bytecode, opcode);
        printOpcode(bytecode, static_cast<Opcode::Type>(opcode));
      }
    }

section_code:
    // skip size of lookup table
    bytecode.seekg(WORD_SIZE, bytecode.cur);
    while (true) {
      READ_INT(bytecode, opcode);
      if (bytecode.eof() || bytecode.fail()) {
        return;
      }
      printOpcode(bytecode, static_cast<Opcode::Type>(opcode));
    }
    goto read_section;
  }

  void Generator::printOpcode(std::stringstream &bytecode, Opcode::Type opcode) {
    size_t size = bytecode.str().length();
    size_t width = std::ceil(std::log10(size + 1)) + 1;

    switch (opcode) {
      case Opcode::push: {
        READ_INT(bytecode, value);
        WRITE(2, "push 0x" << std::setbase(16) << value << std::setbase(10));
        break;
      }
      case Opcode::call: {
        READ_INT(bytecode, nargs);
        WRITE(2, "call (" << nargs << ")");
        break;
      }
      case Opcode::load_string: {
        READ_INT(bytecode, stringID);
        WRITE(2, "load_string $" << stringID);
        break;
      }
      case Opcode::lookup: {
        READ_INT(bytecode, id);
        READ_INT(bytecode, cacheSlot);
        WRITE(3, "lookup $" << id << "(" << strings[id] << ") [cacheSlot=" << cacheSlot << "]");
        break;
      }
      case Opcode::create_closure: {
        READ_INT(bytecode, fnID);
        READ_INT(bytecode, capturesScope);
        WRITE(3, "create_closure " << functions[fnID] << " [capturesScope=" << (capturesScope ? "true" : "false") << "]");
        break;
      }
      case Opcode::jmp:  {
        READ_INT(bytecode, target);
        WRITE(2, "jmp [" << ((int)bytecode.tellg() - (2 * WORD_SIZE) + target) << "]");
        break;
      }
      case Opcode::jz: {
        READ_INT(bytecode, target);
        WRITE(2, "jz [" << ((int)bytecode.tellg() - (2 * WORD_SIZE) + target) << "]");
        break;
      }
      case Opcode::push_arg: {
        READ_INT(bytecode, arg);
        WRITE(2, "push_arg $" << arg);
        break;
      }
      case Opcode::put_to_scope: {
        READ_INT(bytecode, arg);
        WRITE(2, "put_to_scope $" << strings[arg]);
        break;
      }
      case Opcode::bind:
        READ_INT(bytecode, stringID);
        WRITE(2, "bind $" << strings[stringID]);
        break;
      case Opcode::alloc_obj:
        {
          READ_INT(bytecode, size);
          READ_INT(bytecode, tag);
          WRITE(3, "alloc_obj (size=" << size << ", tag=" << tag << ")");
        }
        break;
      case Opcode::alloc_list:
        {
          READ_INT(bytecode, size);
          WRITE(2, "alloc_list (size=" << size << ")");
          break;
        }
      case Opcode::obj_store_at:
        {
          READ_INT(bytecode, index);
          WRITE(2, "obj_store_at #" << index);
          break;
        }
      case Opcode::obj_tag_test:
        READ_INT(bytecode, tag);
        WRITE(2, "obj_tag_test #" << tag);
        break;
      case Opcode::obj_load:
        READ_INT(bytecode, offset);
        WRITE(2, "obj_load #" << offset);
        break;
      case Opcode::stack_alloc: {
        READ_INT(bytecode, size);
        WRITE(2, "stack_alloc #" << size);
        break;
      }
      case Opcode::stack_store: {
        READ_INT(bytecode, slot);
        WRITE(2, "stack_store #" << slot);
        break;
      }
      case Opcode::stack_load: {
        READ_INT(bytecode, slot);
        WRITE(2, "stack_load #" << slot);
        break;
      }
      case Opcode::stack_free: {
        READ_INT(bytecode, size);
        WRITE(2, "stack_free #" << size);
        break;
      }
      case Opcode::ret: {
        WRITE(1, "ret");
        break;
      }
      case Opcode::create_lex_scope: {
        WRITE(1, "create_lex_scope");
        break;
      }
      case Opcode::release_lex_scope: {
        WRITE(1, "release_lex_scope");
        break;
      }
      case Opcode::exit:
        WRITE(1, "exit");
        break;
      default:
        std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
        throw;
    }
  }
#undef WRITE

namespace AST {

void Number::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::push);
  gen->write(value);
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
  gen->emitOpcode(Opcode::lookup);
  auto name = namespaced(ns, this->name);
  gen->write(gen->uniqueString(name));
  if (gen->capturesScope) {
    gen->write(0);
  } else {
    gen->write(gen->lookupID++);
  }
}

void String::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::load_string);
  gen->write(gen->uniqueString(name));
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
  body->generateBytecode(gen);
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

void ObjectTagTest::generateBytecode(Generator *gen) {
  object->generateBytecode(gen);
  gen->emitOpcode(Opcode::obj_tag_test);
  gen->write(tag);
}

void ObjectLoad::generateBytecode(Generator *gen) {
  object->generateBytecode(gen);
  gen->emitOpcode(Opcode::obj_load);
  gen->write(offset);
}

void StackStore::generateBytecode(Generator *gen) {
  value->generateBytecode(gen);
  gen->emitOpcode(Opcode::stack_store);
  gen->write(slot);
}

void StackLoad::generateBytecode(Generator *gen) {
  gen->emitOpcode(Opcode::stack_load);
  gen->write(slot);
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


    for (int j = kase->pattern->stores.size() - 1; j >= 0; j--) {
      kase->body->nodes.insert(kase->body->nodes.begin(), kase->pattern->stores[j]);
    }

    auto jmp = i < size - 1;
    gen->emitJmp(Opcode::jz, kase->body, jmp);
    if (jmp) {
      gen->emitOpcode(Opcode::jmp);
      pos[i] = gen->m_output.tellp();
      gen->write(0);
    }
  }

  auto p = gen->m_output.tellp();
  for (unsigned i = 0; i < size - 1; i++) {
    gen->m_output.seekp(pos[i] );
    unsigned off = p - pos[i];
    gen->write(off + WORD_SIZE);
  }
  gen->m_output.seekp(p);
}

void Let::generateBytecode(Generator *gen) {
  for (auto store : stores) {
    store->generateBytecode(gen);
  }

  for (auto load : loads) {
    if (load->isCaptured) {
      gen->emitOpcode(Opcode::stack_load);
      gen->write(load->slot);

      gen->emitOpcode(Opcode::put_to_scope);
      gen->write(gen->uniqueString(load->name));
    }
  }

  block->generateBytecode(gen);
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
  gen->emitOpcode(Opcode::create_closure);
  gen->write(gen->m_functions.size());
  gen->write(capturesScope);
  if (name != "_") {
    gen->emitOpcode(Opcode::bind);
    auto name = namespaced(ns, this->name);
    gen->write(gen->uniqueString(name));
  }
  gen->m_functions.push_back(this);
}

}
}
