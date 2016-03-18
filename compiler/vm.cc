#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>
#include <iostream>

#define STR_MASK 0x8000000000000000
#define ARRAY_MASK 0x4000000000000000

#define MASK(MASK, V) ((V) | (MASK))
#define UNMASK(MASK, V) ((V) & ~(MASK))
#define IS_MASK(MASK, V) ((V) & (MASK))

#define MASK_STR(STR) MASK(STR_MASK, STR)
#define UNMASK_STR(STR) UNMASK(STR_MASK, STR)
#define IS_STR(STR) IS_MASK(STR_MASK, STR)

#define MASK_ARRAY(ARRAY) MASK(ARRAY_MASK, ARRAY)
#define UNMASK_ARRAY(ARRAY) UNMASK(ARRAY_MASK, ARRAY)
#define IS_ARRAY(ARRAY) IS_MASK(ARRAY_MASK, ARRAY)

#define JS_FUNCTION(FN_NAME) static uintptr_t FN_NAME(ceos::VM &vm, unsigned argv)

#define EACH_ARG(IT) \
  for (uintptr_t I = 0, IT; (IT = vm.arg(I)), I < argv; I++)

#define BASIC_MATH(NAME, OP) \
  JS_FUNCTION(NAME) { \
    assert(argv == 2); \
 \
    return vm.arg(0) OP vm.arg(1); \
  }

BASIC_MATH(add, +)
BASIC_MATH(sub, -)
BASIC_MATH(mul, *)
BASIC_MATH(div, /)

JS_FUNCTION(print) {
  for (unsigned i = 0; i < argv; i++) {
    uintptr_t arg = vm.arg(i);
    if (IS_STR(arg)) {
      std::cout << (reinterpret_cast<std::string *>(UNMASK_STR(arg)))->c_str() << "\n";
    } else if (IS_ARRAY(arg)) {
      std::vector<uintptr_t> *array = reinterpret_cast<__typeof__(array)>(UNMASK_ARRAY(arg));
      for (auto a : *array) {
        std::cout << a << " ";
      }
      std::cout << "\n";
    } else {
      std::cout << static_cast<int>(arg) << "\n";
    }
  }
  
  return 0;
}

JS_FUNCTION(list) {
  auto list = new std::vector<uintptr_t>();
  EACH_ARG(arg) {
    list->push_back(arg);
  }
  return MASK_ARRAY(reinterpret_cast<uintptr_t>(list));
}

#undef BASIC_MATH

namespace ceos {

  void VM::registerBuiltins() {
#define REGISTER(NAME) uintptr_t (*NAME##_)(VM &, unsigned) = NAME; m_functionTable[#NAME] = (uintptr_t)NAME##_

    REGISTER(print);
    REGISTER(add);
    REGISTER(sub);
    REGISTER(mul);
    REGISTER(div);
    REGISTER(list);

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

  uintptr_t VM::arg(unsigned index) {
    // 2 because the pointers are always 1 ahead and ebp is on top of the stack
    return stack[ebp - index - 2];
  }

  void VM::execute() {
    READ_INT(m_bytecode, ceos);

    // section marker
    assert(ceos == Section::Header);

    while (true) {
      READ_INT(m_bytecode, section);

      switch (section) {
        case Section::Strings:
          loadStrings();
          break;
        case Section::Functions:
          loadFunctions();
          break;
        case Section::Text:
          run(m_bytecode);
          break;
      }
    }
  } 

  void VM::loadStrings() {
    while (true) {
      READ_INT(m_bytecode, header);

      if (header == Section::Header) {
        break;
      }

      m_bytecode.seekg(-4, m_bytecode.cur);
      READ_STR(m_bytecode, str);
      m_stringTable.push_back(str);
    }
  }

  void VM::loadFunctions() {
    READ_INT(m_bytecode, initialHeader);
    assert(initialHeader == Section::FunctionHeader);

    while (true) {
      READ_STR(m_bytecode, name);
      READ_INT(m_bytecode, nargs);

      m_userFunctions.push_back(Function(name, nargs));
      Function *fn = &m_userFunctions.back();
      m_functionTable[name] = MASK_STR(reinterpret_cast<uintptr_t>(fn));

      while (true) {
        READ_INT(m_bytecode, opcode);
        if (opcode == Section::Header) {
          return;
        } else if (opcode == Section::FunctionHeader) {
          break;
        }

        fn->bytecode.write(reinterpret_cast<char *>(&opcode), sizeof(opcode));
      }
    }
  }

  void VM::run(std::stringstream &bytecode) {
    while (true) {
      READ_INT(bytecode, opcode);

      switch (opcode) {
        case Opcode::push: {
          READ_INT(bytecode, value);
          stack_push(value);
          break;
        }
        case Opcode::call: {
          int nargs = stack_pop();
          uintptr_t fn_address = stack_pop();

          //assert(fn != nullptr);
          --nargs; //pop'd the callee

          stack_push(ebp);
          ebp = esp;

          uintptr_t ret;
          if (IS_STR(fn_address)) {
            Function *fn = reinterpret_cast<__typeof__(fn)>(UNMASK_STR(fn_address));
            ret = (*fn)(*this, nargs);
          } else {
            uintptr_t (*fn)(VM &, unsigned) = reinterpret_cast<__typeof__(fn)>(fn_address);
            ret = fn(*this, nargs);
          }

          esp = ebp;
          ebp = stack_pop();
          stack.resize(stack.size() - nargs);
          esp -= nargs;

          stack_push(ret);

          break;
        }
        case Opcode::load_string: {
          READ_INT(bytecode, stringID);
          stack_push(MASK_STR(reinterpret_cast<uintptr_t>(&m_stringTable[stringID])));
          break;
        }
        case Opcode::lookup: {
          READ_INT(bytecode, id);
          auto fnName = m_stringTable[id];
          stack_push(reinterpret_cast<uintptr_t>(m_functionTable[fnName]));
          break;
        }
        case Opcode::jmp:  {
          READ_INT(bytecode, target);
          bytecode.seekg(target, bytecode.cur);
          break;
        }
        case Opcode::jz: {
          READ_INT(bytecode, target);
          int value = static_cast<int>(stack_pop());
          if (value == 0) {
            bytecode.seekg(target, bytecode.cur);
          }
          break;
        }
        case Opcode::push_arg: {
          READ_INT(bytecode, index);
          stack_push(arg(index));
          break;
        }
        default:
          std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
          exit(1);
      }
    }
  }

}
