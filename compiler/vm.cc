#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>

namespace ceos {

extern "C" void execute(
    const uint8_t *bytecode,
    char **stringTable,
    VM *vm);

extern "C" uint64_t getScope(VM *vm, char *name);
uint64_t getScope(VM *vm, char *name) {
  auto value = vm->m_scope->get(name);
  if (value.isUndefined()) {
    std::cerr << "Symbol not found: " << name << "\n";
    throw;
  }
  return value.encode();
}

extern "C" void pushScope();
void pushScope() { }

extern "C" uint64_t prepareClosure(unsigned argc, Value *args, Value fnAddress, VM *vm);
uint64_t prepareClosure(unsigned argc, Value *args, Value fnAddress, VM *vm) {
  auto closure = fnAddress.asClosure();
  vm->m_scope = vm->m_scope->create(closure->scope);

  for (unsigned i = 0; i < argc; i++) {
    vm->m_scope->set(closure->fn->arg(i), args[i]);
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
          ::ceos::execute(m_bytecode + pc, m_stringTable.data(), this);
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
      char *str = readStr();
      m_stringTable.push_back(str);
    }
  }

  void VM::loadFunctions() {
    auto initialHeader = read<unsigned>();
    assert(initialHeader == Section::FunctionHeader);

    while (true) {
      auto fnid = read<unsigned>();
      auto nargs = read<unsigned>();

      std::vector<char *> args;
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
