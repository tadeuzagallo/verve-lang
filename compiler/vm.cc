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

#define JS_FUNCTION(FN_NAME) __used static uintptr_t FN_NAME(ceos::VM &vm, unsigned argv)

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
BASIC_MATH(lt, <)
BASIC_MATH(gt, >)
BASIC_MATH(lte, <=)
BASIC_MATH(gte, >=)
BASIC_MATH(equals, ==)

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
#define REGISTER(NAME) JSFunctionType NAME##_ = NAME; m_functionTable[#NAME] = (uintptr_t)NAME##_

    REGISTER(print);
    REGISTER(list);

    REGISTER(add);
    REGISTER(sub);
    REGISTER(mul);
    REGISTER(div);
    REGISTER(lt);
    REGISTER(gt);
    REGISTER(lte);
    REGISTER(gte);
    REGISTER(equals);

#undef REGISTER
  }

  void VM::execute() {
    auto header = read<int>();

    // section marker
    assert(header == Section::Header);

    while (true) {
      auto section = read<int>();

      if (pc > length) {
        return;
      }

      switch (section) {
        case Section::Strings:
          loadStrings();
          break;
        case Section::Functions:
          loadFunctions();

          for (unsigned i = 0; i < m_userFunctions.size(); i++) {
            Function *fn = &m_userFunctions[i];
            m_functionTable[fn->name] = MASK_STR(reinterpret_cast<uintptr_t>(fn));
          }

          break;
        case Section::Text:
          run();
          break;
      }
    }
  } 

  void VM::loadStrings() {
    while (true) {
      auto header = read<int>();

      if (header == Section::Header) {
        break;
      }

      pc -= 4;
      char *str = readStr();
      m_stringTable.push_back(str);
    }
  }

  void VM::loadFunctions() {
    auto initialHeader = read<int>();
    assert(initialHeader == Section::FunctionHeader);

    while (true) {
      char *name = readStr();
      auto nargs = read<int>();

      m_userFunctions.push_back(Function(name, nargs, pc));

      while (true) {
        auto opcode = read<int>();
        if (opcode == Section::Header) {
          return;
        } else if (opcode == Section::FunctionHeader) {
          break;
        }
      }
    }
  }

  void VM::run() {
    while (true) {
      auto opcode = read<int>();

      if (pc > length) {
        return;
      }

      switch (opcode) {
        case Opcode::push: {
          auto value = read<int>();
          stack_push(value);
          break;
        }
        case Opcode::call: {
          int nargs = stack_pop();
          uintptr_t fn_address = stack_pop();

          --nargs; //pop'd the callee

          stack_push(pc);
          stack_push(nargs);
          stack_push(ebp);
          ebp = esp;

          uintptr_t ret;
          if (IS_STR(fn_address)) {
            Function *fn = reinterpret_cast<__typeof__(fn)>(UNMASK_STR(fn_address));
            pc = fn->offset;
            break;
          } else {
            JSFunctionType fn = reinterpret_cast<__typeof__(fn)>(fn_address);
            ret = fn(*this, nargs);
            stack_push(ret);
          }
        }
        case Opcode::ret: {
          uintptr_t ret = stack_pop();

          esp = ebp;

          ebp = stack_pop();
          unsigned nargs = stack_pop();
          auto ret_addr = stack_pop();

          esp -= nargs;

          stack_push(ret);
          pc = ret_addr;
          break;
        }
        case Opcode::load_string: {
          auto stringID = read<int>();
          stack_push(MASK_STR(reinterpret_cast<uintptr_t>(&m_stringTable[stringID])));
          break;
        }
        case Opcode::lookup: {
          auto id = read<int>();
          auto fnName = m_stringTable[id];
          auto fnAddress = m_functionTable[fnName];
          stack_push(fnAddress);
          break;
        }
        case Opcode::jmp:  {
          auto target = read<int>();
          pc += target;
          break;
        }
        case Opcode::jz: {
          auto target = read<int>();
          int value = static_cast<int>(stack_pop());
          if (value == 0) {
            pc += target;
          }
          break;
        }
        case Opcode::push_arg: {
          auto index = read<int>();
          stack_push(arg(index));
          break;
        }
        default:
          std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
          throw;
      }
    }
  }

}
