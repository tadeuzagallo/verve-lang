#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>

namespace ceos {

extern "C" void execute(
    const uint8_t *bytecode,
    String *stringTable,
    VM *vm,
    const uint8_t *bcbase);

extern "C" void setScope(VM *vm, const char *name, Value closure);
void setScope(VM *vm, const char *name, Value closure) {
  vm->m_scope->set(name, closure);
}

extern "C" void restoreScope(VM *vm);
void restoreScope(VM *vm) {
  vm->m_scope = vm->m_scope->restore();
}

extern "C" uint64_t createClosure(VM *vm, unsigned fnID);
uint64_t createClosure(VM *vm, unsigned fnID) {
  auto closure = new Closure(vm->m_scope);
  vm->trackAllocation(closure, sizeof(Closure));
  closure->fn = &vm->m_userFunctions[fnID];
  return Value(closure).encode();
}

extern "C" unsigned prepareClosure(unsigned argc, Value *argv, VM *vm, Closure *closure);
unsigned prepareClosure(unsigned argc, Value *argv, VM *vm, Closure *closure) {
  vm->m_scope = vm->m_scope->create(closure->scope);

  for (unsigned i = 0; i < argc; i++) {
    vm->m_scope->set(closure->fn->args[i], argv[i]);
  }

  return closure->fn->offset;
}

  void VM::execute() {
    auto header = read<unsigned>();

    // section marker
    assert(header == Section::Header);

    while (true) {
      auto section = read<unsigned>();

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
          ::ceos::execute(m_bytecode + pc, m_stringTable.data(), this, m_bytecode);
          return;
        default:
          std::cerr << "Unknown section: `0x0" << std::hex << section << "`\n";
          throw;
      }
    }
  }

  void VM::loadStrings() {
    while (true) {
      auto header = read<unsigned>();

      if (header == Section::Header) {
        break;
      }

      pc -= sizeof(header);
      String str = readStr();
      m_stringTable.push_back(str);
    }
  }

  void VM::loadFunctions() {
    auto initialHeader = read<unsigned>();
    assert(initialHeader == Section::FunctionHeader);

    while (true) {
      auto fnid = read<unsigned>();
      auto nargs = read<unsigned>();

      std::vector<String> args;
      for (unsigned i = 0; i < nargs; i++) {
        auto argID = read<unsigned>();
        args.push_back(m_stringTable[argID]);
      }
      m_userFunctions.push_back(Function(fnid, nargs, pc, std::move(args)));

      while (true) {
        auto opcode = read<unsigned>();
        if (opcode == Section::Header) {
          return;
        } else if (opcode == Section::FunctionHeader) {
          break;
        }
        pc -= 3;
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

    // TODO: walk the actual stack
    //for (auto value : stack) {
      //GC::markValue(value, blocks);
    //}

    GC::markScope(m_scope, blocks);

    GC::sweep(blocks, &heapSize);
  }

}
