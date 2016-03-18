#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>

#ifndef CEOS_VM_H
#define CEOS_VM_H

namespace ceos {
  class VM;

  typedef std::binary_function<VM &, unsigned, uintptr_t> JSFunctionType;

  class VM {
    public:
      VM(std::stringstream &bytecode): m_bytecode(bytecode) {
        registerBuiltins();
      }

      void registerBuiltins();
      void execute();
      void loadStrings();
      void loadFunctions();
      void run(std::stringstream &);
      void stack_push(uintptr_t);
      uintptr_t stack_pop();
      uintptr_t arg(unsigned);
      
      std::vector<uintptr_t> stack;
      uintptr_t ebp;
      uintptr_t esp;

    private:

      struct Function : JSFunctionType {
        Function(std::string n, unsigned args) : name(n), nargs(args) {}

        uintptr_t operator()(VM &vm, __unused unsigned argv) {
          vm.run(bytecode);
          return vm.stack_pop();
        }

        std::string name;
        unsigned nargs;
        std::stringstream bytecode;
      };

      std::stringstream &m_bytecode;
      std::vector<std::string> m_stringTable;
      std::unordered_map<std::string, uintptr_t> m_functionTable;
      std::vector<Function> m_userFunctions;
  };
}

#endif
