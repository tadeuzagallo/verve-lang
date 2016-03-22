#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>

namespace ceos {

#define JS_FUNCTION(FN_NAME) __used static Value FN_NAME(ceos::VM &vm, unsigned argv)

#define EACH_ARG(IT) \
  Value IT; for (unsigned I = 0; (I < argv ? (IT = vm.arg(I)) : 0), I < argv; I++)

#define BASIC_MATH(NAME, OP) \
  JS_FUNCTION(NAME) { \
    assert(argv == 2); \
 \
    return Value(vm.arg(0).asInt() OP vm.arg(1).asInt()); \
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
    Value arg = vm.arg(i);
    if (arg.isString()) {
      std::cout << arg.asString()->c_str();
    } else if (arg.isArray()) {
      for (auto a : *arg.asArray()) {
        std::cout << a.asInt() << " ";
      }
    } else {
      std::cout << arg.asInt();
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

  Value arg = vm.arg(0);
  if (arg.isString()) {
    return arg.asString()->c_str()[vm.arg(1).asInt()];
  } else if (arg.isArray()) {
    auto array = arg.asArray();
    return array->at(vm.arg(1).asInt());
  }

  return Value(0);
}

JS_FUNCTION(substr) {
  assert(argv == 2 || argv == 3);

  Value arg = vm.arg(0);
  if (arg.isString()) {
    std::string *str = arg.asString();
    std::string substring;
    if (argv == 2) {
      substring = str->substr(vm.arg(1).asInt());
    } else {
      substring = str->substr(vm.arg(1).asInt(), vm.arg(2).asInt());
    }
    auto s = new std::string(substring);
    return Value(s);
  } else {
    throw;
  }

  return 0;
}

JS_FUNCTION(count) {
  assert(argv == 1);

  Value arg = vm.arg(0);
  if (arg.isString()) {
    return arg.asString()->length();
  } else {
    throw;
  }

  return 0;
}

JS_FUNCTION(list) {
  auto list = new std::vector<Value>();
  EACH_ARG(arg) {
    list->push_back(arg);
  }
  return Value(list);
}

#undef BASIC_MATH

  void VM::registerBuiltins() {
#define REGISTER(NAME, FN) Builtin NAME##_ = FN; m_scope->set(#NAME, Value(NAME##_))

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
          Value fn_address = stack_pop();

          stack_push(pc);
          stack_push(nargs);
          stack_push(ebp);
          ebp = esp;

          if (fn_address.isClosure()) {
            auto closure = fn_address.asClosure();

            m_scope = m_scope->create(closure->scope);

            for (unsigned i = 0; i < nargs; i++) {
              m_scope->set(closure->fn->arg(i), arg(i));
            }

            pc = closure->fn->offset;
            break;
          } else {
            m_scope = m_scope->create();

            Builtin fn = fn_address.asBuiltin();
            Value ret = fn(*this, nargs);
            stack_push(ret);
          }
        }
        case Opcode::ret: {
          Value ret = stack_pop();

          esp = ebp;

          ebp = stack_pop().asInt();
          unsigned nargs = stack_pop().asInt();
          auto ret_addr = stack_pop().asInt();

          esp -= nargs;

          stack_push(ret);
          pc = ret_addr;

          m_scope = m_scope->restore();

          break;
        }
        case Opcode::load_string: {
          auto stringID = read<int>();
          stack_push(Value(&m_stringTable[stringID]));
          break;
        }
        case Opcode::lookup: {
          auto id = read<int>();
          auto name = m_stringTable[id];
          auto value = m_scope->get(name);
          if (value.isUndefined()) {
            std::cerr << "Symbol not found: " << name << "\n";
            throw;
          }
          stack_push(value);
          break;
        }
        case Opcode::jmp:  {
          auto target = read<int>();
          pc += target;
          break;
        }
        case Opcode::jz: {
          auto target = read<int>();
          int value = stack_pop().asInt();
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
        case Opcode::create_closure: {
          auto fnID = read<int>();
          auto closure = new Closure();
          closure->fn = &m_userFunctions[fnID];
          closure->scope = m_scope;
          stack_push(Value(closure));
          break;
        }
        case Opcode::bind: {
          auto address = stack_pop();
          auto closure = address.asClosure();
          m_scope->set(closure->fn->name(this), address);
          break;
        }
        default:
          std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
          throw;
      }
    }
  }

}
