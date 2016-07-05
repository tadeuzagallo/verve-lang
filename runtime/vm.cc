#include "vm.h"

#include "bytecode/opcodes.h"
#include "bytecode/sections.h"

#include <cassert>

namespace Verve {

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

extern "C" void tagTestFailed(unsigned, unsigned);
void tagTestFailed(unsigned actual, unsigned expected) {
  fprintf(stderr, "Invalid pattern match: Object has tag `%u` but expected tag `%u`\n", actual, expected);
  throw;
}

extern "C" uintptr_t allocate(VM *vm, unsigned size);
uintptr_t allocate(VM *vm, unsigned size) {
  auto address = calloc(size, 8);
  vm->trackAllocation(address, size);
  return reinterpret_cast<uintptr_t>(address);
}

  void VM::execute() {
    auto header = read<uint64_t>();
    assert(header == Section::Header);

    loadStrings();
    loadFunctions();
    loadText();
  }

  inline void VM::loadStrings() {
    auto header = read<uint64_t>();
    if (header != Section::Strings) {
      pc -= WORD_SIZE;
      return;
    }

    while (true) {
      auto header = read<uint64_t>();

      if (header == Section::Header) {
        break;
      }

      pc -= sizeof(header);
      String str = readStr();
      m_stringTable.push_back(str);

      while (m_bytecode[pc] == '\1') pc++;
    }
  }

  inline void VM::loadFunctions() {
    auto header = read<uint64_t>();
    if (header != Section::Functions) {
      pc -= WORD_SIZE;
      return;
    }

    auto initialHeader = read<uint64_t>();
    assert(initialHeader == Section::FunctionHeader);

    while (true) {
      auto fnid = read<uint64_t>();
      auto nargs = read<uint64_t>();

      std::vector<String> args;
      for (unsigned i = 0; i < nargs; i++) {
        auto argID = read<uint64_t>();
        args.push_back(m_stringTable[argID]);
      }
      m_userFunctions.push_back(Function(fnid, nargs, pc, std::move(args)));

      linkBytecode();
      while (true) {
        auto opcode = read<uint64_t>();
        if (opcode == Section::Header) {
          return;
        } else if (opcode == Section::FunctionHeader) {
          break;
        }
      }
    }
  }

  inline void VM::loadText()  {
    auto header = read<uint64_t>();
    if (header != Section::Text) {
      pc -= WORD_SIZE;
      return;
    }

    auto lookupTableSize = read<uint64_t>();
    void *lookupTable = calloc(lookupTableSize * WORD_SIZE, 1);
    linkBytecode();
    ::Verve::execute(m_bytecode + pc, &m_stringTable[0], this, m_bytecode, lookupTable);
  }

  void VM::linkBytecode() {
    if (m_needsLinking) {
      auto bytecode = (uint64_t *)m_bytecode;
      for (auto i = pc; i < length; i += WORD_SIZE) {
        auto value = bytecode[i / WORD_SIZE];
        if (value == Section::Header || value == Section::FunctionHeader) {
          return;
        }
        auto opcode = (Opcode::Type)value;
        bytecode[i / WORD_SIZE] = Opcode::address(opcode);
        i += Opcode::size(opcode) * WORD_SIZE;
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

    volatile void **rsp;
    asm("movq %%rsp, %0" : "=r"(rsp));

    pthread_t self = pthread_self();
    void *stackBottom = pthread_get_stackaddr_np(self);
    while (rsp != stackBottom) {
      GC::markValue(Value::decode((uintptr_t)*rsp), blocks);
      rsp++;
    }

    GC::markScope(m_scope, blocks);

    GC::sweep(blocks, &heapSize);
  }

}
