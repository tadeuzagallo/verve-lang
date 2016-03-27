#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>

namespace ceos {

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
          trackAllocation(closure, sizeof(Closure));
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

  void VM::trackAllocation(void *ptr, size_t size) {
    heapSize += size;

    if (heapSize > heapLimit) {
      collect();
      heapLimit = std::max(heapLimit, 2 * heapSize);
    }

    blocks.push_back(std::make_pair(size, ptr));
  }

  void VM::collect() {
    GC::start();

    for (auto value : stack) {
      GC::markValue(value, blocks);
    }

    GC::markScope(m_scope, blocks);

    GC::sweep(blocks, &heapSize);
  }

}
