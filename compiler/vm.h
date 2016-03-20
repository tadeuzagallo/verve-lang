#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>

#ifndef CEOS_VM_H
#define CEOS_VM_H

namespace ceos {
  class VM;

  typedef uintptr_t (*JSFunctionType)(VM &, unsigned);

  class VM {
    public:
      VM(std::stringstream &bs):
        stack(2048),
        pc(0) {
        std::string bc = bs.str();
        length = bc.length();
        m_bytecode = (uint8_t *)malloc(length);
        memcpy(m_bytecode, bc.data(), length);
        registerBuiltins();
      }

      void registerBuiltins();
      void execute();
      void loadStrings();
      void loadFunctions();
      void run();

      void stack_push(uintptr_t value) {
        if (esp > stack.size() - 2) {
          std::cerr << "Stack overflow\n";
          throw;
        }
        stack[esp++] = value;
      }

      uintptr_t stack_pop() {
        return stack[--esp];
      }

      uintptr_t arg(unsigned index) {
        return stack[ebp - index - 4];
      }

      template<typename T>
      T read() {
        T v = *(T *)(m_bytecode + pc);
        pc += sizeof(T);
        return v;
      }

      char *readStr() {
        char *v = (char *)(m_bytecode + pc);
        pc += strlen(v) + 1;
        return v;
      }
      
      std::vector<uintptr_t> stack;
      uintptr_t ebp;
      uintptr_t esp;
      uintptr_t pc;
      size_t length;

    private:

      struct Function {
        Function(unsigned i, unsigned args, unsigned o) : offset(o), id(i), nargs(args) {}

        std::string &name(VM *vm) {
          return vm->m_stringTable[id];
        }

        unsigned offset;
        unsigned id;
        unsigned nargs;
      };

      uint8_t *m_bytecode;
      std::vector<std::string> m_stringTable;
      std::unordered_map<std::string, uintptr_t> m_functionTable;
      std::vector<Function> m_userFunctions;
  };
}

#endif
