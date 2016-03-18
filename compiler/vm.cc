#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>
#include <iostream>

#define MASK 0x8000000000000000
#define MASK_STR(STR) ((STR) | (MASK))
#define UNMASK_STR(STR) ((STR) & ~(MASK))
#define IS_STR(STR) ((STR) & (MASK))

#define JS_FUNCTION(FN_NAME) static uintptr_t FN_NAME(ceos::VM &vm, unsigned argv)

#define ARG(index) vm.stack[vm.ebp - index - 2]

#define BASIC_MATH(NAME, OP) \
  JS_FUNCTION(NAME) { \
    assert(argv == 2); \
 \
    return ARG(0) OP ARG(1); \
  }

BASIC_MATH(add, +)
BASIC_MATH(sub, -)
BASIC_MATH(mul, *)
BASIC_MATH(div, /)

JS_FUNCTION(print) {
  for (unsigned i = 0; i < argv; i++) {
    uintptr_t arg = ARG(i);
    if (IS_STR(arg)) {
      std::cout << (reinterpret_cast<std::string *>(UNMASK_STR(arg)))->c_str() << "\n";
    } else {
      std::cout << static_cast<int>(arg) << "\n";
    }
  }
  
  return 0;
}

#undef BASIC_MATH

namespace ceos {

  void VM::registerBuiltins() {
#define REGISTER(NAME) m_functionTable[#NAME] = NAME

    REGISTER(print);
    REGISTER(add);
    REGISTER(sub);
    REGISTER(mul);
    REGISTER(div);

#undef REGISTER
  }

  void VM::stack_push(uintptr_t value) {
    esp++;
    stack.push_back(value);
  }

  uintptr_t VM::stack_pop() {
    uintptr_t value = stack.back();
    stack.pop_back();
    esp--;
    return value;
  }

#define READ_INT(INT_NAME) \
      int INT_NAME; \
      m_bytecode.read(reinterpret_cast<char *>(&INT_NAME), sizeof(INT_NAME)); \

#define READ_STR(STR_NAME) \
    std::stringstream STR_NAME##_; \
    m_bytecode.get(*STR_NAME##_.rdbuf(), '\0'); \
    std::string STR_NAME = STR_NAME##_.str(); \
    m_bytecode.ignore(1);

  void VM::execute() {
    READ_INT(ceos);

    // section marker
    assert(ceos == Section::Header);

    while (true) {
      READ_INT(section);

      if (m_bytecode.eof()) break;

      switch (section) {
        case Section::Strings:
          loadStrings();
          break;
        case Section::Text:
          run();
          break;
        case Section::Functions:
          assert(0);
      }
    }
  } 

  void VM::loadStrings() {
    while (true) {
      READ_INT(header);

      if (header == Section::Header) {
        break;
      }

      m_bytecode.seekg(-4, m_bytecode.cur);
      READ_STR(str);
      m_stringTable.push_back(str);
    }
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

          uintptr_t (*fn)(VM &, unsigned) = reinterpret_cast<__typeof__(fn)>(stack_pop());
          assert(fn != nullptr);
          --nargs; //pop'd the callee

          stack_push(ebp);
          ebp = esp;

          auto ret = fn(*this, nargs);

          esp = ebp;
          ebp = stack_pop();
          stack.resize(stack.size() - nargs);
          esp -= nargs;

          stack_push(ret);

          break;
        }
        case Opcode::load_string: {
          READ_INT(stringID);
          stack_push(MASK_STR(reinterpret_cast<uintptr_t>(&m_stringTable[stringID])));
          break;
        }
        case Opcode::lookup: {
          std::string *fnName = reinterpret_cast<std::string *>(UNMASK_STR(stack_pop()));
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
      }
    }
  }
#undef READ_INT

}
