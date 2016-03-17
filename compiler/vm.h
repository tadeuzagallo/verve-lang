#include <sstream>
#include <vector>
#include <unordered_map>

#ifndef CEOS_VM_H
#define CEOS_VM_H

namespace ceos {
  class VM {
    public:
      VM(std::stringstream &bytecode): m_bytecode(bytecode) {
        registerBuiltins();
      }

      void registerBuiltins();
      void execute();
      void loadStrings();
      void run();
      void stack_push(uintptr_t);
      uintptr_t stack_pop();

    private:
      std::stringstream &m_bytecode;
      std::vector<uintptr_t> m_stack;
      std::vector<std::string> m_stringTable;
      std::unordered_map<std::string, void (*)(VM &, int)> m_functionTable;
  };
}

#endif
