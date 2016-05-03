#include "vm.h"

#include "opcodes.h"
#include "sections.h"

#include <cassert>

extern "C" void op_push();
extern "C" void op_lookup();
extern "C" void op_call();
extern "C" void op_load_string();
extern "C" void op_exit();

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
    auto header = read<uint64_t>();

    // section marker
    assert(header == Section::Header);

    while (true) {
      auto section = read<uint64_t>();

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
          return;
        default:
          std::cerr << "Unknown section: `0x0" << std::hex << section << "`\n";
          throw;
      }
    }
  }

  void VM::loadStrings() {
    while (true) {
      auto header = read<uint64_t>();

      if (header == Section::Header) {
        break;
      }

      pc -= sizeof(header);
      char *str = readStr();
      m_stringTable.push_back(str);
    }
  }

  void VM::loadFunctions() {
    auto initialHeader = read<uint64_t>();
    assert(initialHeader == Section::FunctionHeader);

    while (true) {
      auto fnid = read<unsigned>();
      auto nargs = read<unsigned>();

      std::vector<char *> args;
      for (unsigned i = 0; i < nargs; i++) {
        auto argID = read<uint64_t>();
        args.push_back(m_stringTable[argID]);
      }
      m_userFunctions.push_back(Function(fnid, nargs, pc, std::move(args)));

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

  void VM::run() {
    inflate();
    ::ceos::execute(m_bytecode + pc, m_stringTable.data(), this);
  }

  void VM::inflate() {
    auto _pc = pc;
    while (true) {
      auto opcode = (int)read<uint64_t>();

      if (pc > length) {
        break;
      }

#define LINK(opcode, argc) \
  case Opcode:: opcode : \
    *((uint64_t *)(m_bytecode + pc - 8)) = (uint64_t)op_##opcode; \
    pc += argc * sizeof(uint64_t); \
    break;

      switch (opcode) {
        LINK(push, 1)
        LINK(lookup, 1)
        LINK(call, 1)
        LINK(load_string, 1)
        LINK(exit, 0)
        //LINK(bind, 0)
        default:
          std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
          throw;
      }
    }
    pc = _pc;
  }
    //while (true) {
      //auto opcode = read<int>();

      //if (pc > length) {
        //return;
      //}

      //switch (opcode) {
        //case Opcode::push: {
          //auto value = read<int>();
          //stack_push(value);
          //break;
        //}
        //case Opcode::call: {
          //auto nargs = read<unsigned>();
          //Value fn_address = stack_pop();

          //stack_push(pc);
          //stack_push(nargs);
          //stack_push(ebp);
          //ebp = esp;

          //if (fn_address.isClosure()) {
            //auto closure = fn_address.asClosure();

            //m_scope = m_scope->create(closure->scope);

            //for (unsigned i = 0; i < nargs; i++) {
              //m_scope->set(closure->fn->arg(i), arg(i));
            //}

            //pc = closure->fn->offset;
            //break;
          //} else {
            //m_scope = m_scope->create();

            //Builtin fn = fn_address.asBuiltin();
            //Value ret = fn(*this, nargs);
            //stack_push(ret);
          //}
        //}
        //case Opcode::ret: {
          //Value ret = stack_pop();

          //esp = ebp;

          //ebp = stack_pop().asInt();
          //unsigned nargs = stack_pop().asInt();
          //auto ret_addr = stack_pop().asInt();

          //esp -= nargs;

          //stack_push(ret);
          //pc = ret_addr;

          //m_scope = m_scope->restore();

          //break;
        //}
        //case Opcode::load_string: {
          //auto stringID = read<int>();
          //stack_push(Value(&m_stringTable[stringID]));
          //break;
        //}
        //case Opcode::lookup: {
          //auto id = read<int>();
          //auto name = m_stringTable[id];
          //auto value = m_scope->get(name);
          //if (value.isUndefined()) {
            //std::cerr << "Symbol not found: " << name << "\n";
            //throw;
          //}
          //stack_push(value);
          //break;
        //}
        //case Opcode::jmp:  {
          //auto target = read<int>();
          //pc += target;
          //break;
        //}
        //case Opcode::jz: {
          //auto target = read<int>();
          //int value = stack_pop().asInt();
          //if (value == 0) {
            //pc += target;
          //}
          //break;
        //}
        //case Opcode::push_arg: {
          //auto index = read<int>();
          //stack_push(arg(index));
          //break;
        //}
        //case Opcode::create_closure: {
          //auto fnID = read<int>();
          //auto closure = new Closure();
          //trackAllocation(closure, sizeof(Closure));
          //closure->fn = &m_userFunctions[fnID];
          //closure->scope = m_scope;
          //stack_push(Value(closure));
          //break;
        //}
        //case Opcode::bind: {
          //auto address = stack_pop();
          //auto closure = address.asClosure();
          //m_scope->set(closure->fn->name(this), address);
          //break;
        //}
        //default:
          //std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
          //throw;
      //}
    //}

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
