#include "vm.h"
#include "opcodes.h"

#include <cassert>
#include <iostream>

static void print(ceos::VM &vm, int argv) {
  while (argv--) {
    std::cout << vm.stack_pop() << "\n";
  }
}

namespace ceos {

  void VM::registerBuiltins() {
    m_functionTable["print"] = print;
  }

  void VM::stack_push(uintptr_t value) {
    m_stack.push_back(value);
  }

  uintptr_t VM::stack_pop() {
    uintptr_t value = m_stack.back();
    m_stack.pop_back();
    return value;
  }

#define READ_INT(INT_NAME) \
        int INT_NAME; \
        { \
        union { \
          char c[4]; \
          int i; \
        } tmp; \
        m_bytecode.get(tmp.c[0]); \
        m_bytecode.get(tmp.c[1]); \
        m_bytecode.get(tmp.c[2]); \
        m_bytecode.get(tmp.c[3]); \
        INT_NAME = tmp.i; \
        }

  void VM::execute() {
    while (true) {
      READ_INT(ceos);

      if (m_bytecode.eof()) break;

      // section marker
      assert(ceos == 0xCE05);

      READ_INT(section);
      switch (section) {
        case 0x0001: // strings
          loadStrings();
          break;
        case 0x0002: //text
          run();
          break;
        default:
          assert(0);
      }
    }
  } 

  void VM::loadStrings() {
    const char *str = m_bytecode.str().c_str() + m_bytecode.tellg();
    m_stringTable.push_back(str);
    m_bytecode.seekg(strlen(str) + 1, m_bytecode.cur);
  }

  void VM::run() {
    while (true) {
      READ_INT(opcode);

      if (m_bytecode.eof()) break;

      switch (opcode) {
        case Opcode::push: {
          READ_INT(value);
          stack_push(value);
          break;
        }
        case Opcode::call: {
          int nargs = stack_pop();
          void (*fn)(VM &, int) = reinterpret_cast<__typeof__(fn)>(stack_pop());
          fn(*this, nargs - 1);
          break;
        }
        case Opcode::load_string: {
          READ_INT(stringID);
          stack_push(reinterpret_cast<uintptr_t>(&m_stringTable[stringID]));
          break;
        }
        case Opcode::lookup: {
          std::string *fnName = reinterpret_cast<std::string *>(stack_pop());
          stack_push(reinterpret_cast<uintptr_t>(m_functionTable[*fnName]));
          break;
        }
        case Opcode::jmp:  {
          READ_INT(target);
          m_bytecode.seekg(target, m_bytecode.cur);
          break;
        }
        case Opcode::jz: {
          READ_INT(target);
          int value = static_cast<int>(stack_pop());
          if (value == 0) {
            m_bytecode.seekg(target, m_bytecode.cur);
          }
          break;
        }
        default:
          break;
      }
    }
  }

#undef READ_INT

}
