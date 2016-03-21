#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>
#include <iostream>

#define STR_MASK 0x8000000000000000
#define ARRAY_MASK 0x4000000000000000

#define MASK(MASK, V) ((V) | (MASK##_MASK))
#define UNMASK(MASK, V) ((V) & ~(MASK##_MASK))
#define IS_MASK(MASK, V) ((V) & (MASK##_MASK))

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
BASIC_MATH(_and, &&)
BASIC_MATH(_or, ||)

JS_FUNCTION(print) {
  for (unsigned i = 0; i < argv; i++) {
    uintptr_t arg = vm.arg(i);
    if (IS_MASK(STR, arg)) {
      std::cout << (reinterpret_cast<std::string *>(UNMASK(STR, arg)))->c_str();
    } else if (IS_MASK(ARRAY, arg)) {
      std::vector<uintptr_t> *array = reinterpret_cast<__typeof__(array)>(UNMASK(ARRAY, arg));
      for (auto a : *array) {
        std::cout << a << " ";
      }
    } else {
      std::cout << static_cast<int>(arg);
    }

    if (i < argv - 1) {
      std::cout << " ";
    }
  }
  std::cout << "\n";

  return 0;
}

JS_FUNCTION(at) {
  assert(argv == 2);

  uintptr_t arg = vm.arg(0);
  if (IS_MASK(STR, arg)) {
    return (reinterpret_cast<std::string *>(UNMASK(STR, arg)))->c_str()[vm.arg(1)];
  } else if (IS_MASK(ARRAY, arg)) {
    auto array = reinterpret_cast<std::vector<uintptr_t> *>(UNMASK(ARRAY, arg));
    return array->at(vm.arg(1));
  }

  return 0;
}

JS_FUNCTION(substr) {
  assert(argv == 2 || argv == 3);

  uintptr_t arg = vm.arg(0);
  if (IS_MASK(STR, arg)) {
    std::string *str = reinterpret_cast<std::string *>(UNMASK(STR, arg));
    std::string substring;
    if (argv == 2) {
      substring = str->substr(vm.arg(1));
    } else {
      substring = str->substr(vm.arg(1), vm.arg(2));
    }
    auto s = (std::string *)malloc(sizeof(substring));
    memmove(s, &substring, sizeof(substring));
    return MASK(STR, reinterpret_cast<uintptr_t>(&s));
  } else {
    throw;
  }

  return 0;
}

JS_FUNCTION(count) {
  assert(argv == 1);

  uintptr_t arg = vm.arg(0);
  if (IS_MASK(STR, arg)) {
    std::string *str = reinterpret_cast<std::string *>(UNMASK(STR, arg));
    return str->length();
  } else {
    throw;
  }

  return 0;
}

JS_FUNCTION(list) {
  auto list = new std::vector<uintptr_t>();
  EACH_ARG(arg) {
    list->push_back(arg);
  }
  return MASK(ARRAY, reinterpret_cast<uintptr_t>(list));
}

#undef BASIC_MATH

namespace ceos {

  void VM::registerBuiltins() {
#define REGISTER(NAME, FN) JSFunctionType NAME##_ = FN; m_scope->table[#NAME] = (uintptr_t)NAME##_

    REGISTER(print, print);
    REGISTER(list, list);

    REGISTER(add, add);
    REGISTER(sub, sub);
    REGISTER(mul, mul);
    REGISTER(div, div);
    REGISTER(lt, lt);
    REGISTER(gt, gt);
    REGISTER(lte, lte);
    REGISTER(gte, gte);
    REGISTER(equals, equals);
    REGISTER(and, _and);
    REGISTER(or, _or);
    REGISTER(at, at);
    REGISTER(substr, substr);
    REGISTER(count, count);

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
            m_scope->table[fn->name(this)] = MASK(STR, reinterpret_cast<uintptr_t>(fn));
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
      auto fnid = read<unsigned>();
      auto nargs = read<unsigned>();

      std::vector<std::string *> args;
      for (unsigned i = 0; i < nargs; i++) {
        auto argID = read<unsigned>();
        args.push_back(&m_stringTable[argID]);
      }
      m_userFunctions.push_back(Function(fnid, nargs, pc, std::move(args)));

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
          auto nargs = read<unsigned>();
          uintptr_t fn_address = stack_pop();

          stack_push(pc);
          stack_push(nargs);
          stack_push(ebp);
          ebp = esp;

          uintptr_t ret;
          if (IS_MASK(ARRAY, fn_address)) {
            auto lambda = reinterpret_cast<Lambda *>(UNMASK(ARRAY, fn_address));
            m_scope = std::make_shared<Scope>(lambda->scope, m_scope->parent);
            for (unsigned i = 0; i < nargs; i++) {
              m_scope->table[lambda->fn->arg(i)] = arg(i);
            }

            pc = lambda->fn->offset;
            break;
          } else {
            m_scope = std::make_shared<Scope>(m_scope);

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

          if (m_scope->other) {
            m_scope = m_scope->other;
          } else {
            m_scope = m_scope->parent;
          }
          break;
        }
        case Opcode::load_string: {
          auto stringID = read<int>();
          stack_push(MASK(STR, reinterpret_cast<uintptr_t>(&m_stringTable[stringID])));
          break;
        }
        case Opcode::lookup: {
          auto id = read<int>();
          auto fnAddress = read<uintptr_t>();
          if (!fnAddress) {
            auto fnName = m_stringTable[id];
            fnAddress = m_scope->get(fnName);
            memcpy(m_bytecode + pc - sizeof(uintptr_t), &fnAddress, sizeof(uintptr_t));
          }
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
        case Opcode::create_lambda: {
          auto lambda = new Lambda();
          auto fnAddress = stack_pop();
          assert(IS_MASK(STR, fnAddress));
          lambda->fn = reinterpret_cast<Function *>(UNMASK(STR, fnAddress));
          lambda->scope = m_scope;
          stack_push(MASK(ARRAY, reinterpret_cast<uintptr_t>(lambda)));
          break;
        }
        case Opcode::bind: {
          auto address = stack_pop();
          auto lambda = reinterpret_cast<Lambda *>(UNMASK(ARRAY, address));
          m_scope->table[lambda->fn->name(this)] = address;
          break;
        }
        default:
          std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
          throw;
      }
    }
  }

}
