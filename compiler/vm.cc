#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>

namespace ceos {

extern "C" void execute(
    const uint8_t *bytecode,
    String *stringTable,
    VM *vm,
    const uint8_t *bcbase,
    void *lookupTable);

extern "C" void setScope(VM *vm, const char *name, Value value);
void setScope(VM *vm, const char *name, Value value) {
  vm->m_scope->set(name, value);
}

extern "C" void pushScope(VM *vm);
void pushScope(VM *vm) {
  vm->m_scope = vm->m_scope->create();
}

extern "C" void restoreScope(VM *vm);
void restoreScope(VM *vm) {
  vm->m_scope = vm->m_scope->restore();
}

extern "C" uint64_t createClosure(VM *vm, unsigned fnID, bool capturesScope);
uint64_t createClosure(VM *vm, unsigned fnID, bool capturesScope) {
  if (capturesScope) {
    auto closure = new Closure();
    closure->scope = vm->m_scope->inc();
    vm->trackAllocation(closure, sizeof(Closure));
    closure->fn = &vm->m_userFunctions[fnID];
    return Value(closure).encode();
  } else {
    return Value::fastClosure(vm->m_userFunctions[fnID].offset).encode();
  }
}

extern "C" unsigned prepareClosure(unsigned argc, Value *argv, VM *vm, Closure *closure);
unsigned prepareClosure(__unused unsigned argc, __unused Value *argv, VM *vm, Closure *closure) {
  if (closure->scope != NULL) {
    vm->m_scope = vm->m_scope->create(closure->scope);
  }
  return closure->fn->offset;
}

extern "C" void finishClosure(VM *vm, Closure *closure);
void finishClosure(VM *vm, Closure *closure) {
  if (closure->scope) {
    vm->m_scope = vm->m_scope->restore();
  }
}

extern "C" void symbolNotFound(char *);
void symbolNotFound(char *symbolName) {
  fprintf(stderr, "Symbol not found: %s\n", symbolName);
  throw;
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
        case Section::Text: {
          auto lookupTableSize = read<unsigned>();
          void *lookupTable = calloc(lookupTableSize * 8, 1);
          ::ceos::execute(m_bytecode + pc, m_stringTable.data(), this, m_bytecode, lookupTable);
          return;
        }
        default:
          std::cerr << "Unknown section: `0x0" << std::hex << section << "`\n";
          throw;
      }
    }
  }

  inline void VM::loadStrings() {
    while (true) {
      auto header = read<unsigned>();

      if (header == Section::Header) {
        break;
      }

      pc -= sizeof(header);
      String str = readStr();
      while (m_bytecode[pc] == '\0') pc++;
      m_stringTable.push_back(str);
    }
  }

  inline void VM::loadFunctions() {
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
