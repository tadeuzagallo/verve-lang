#include "closure.h"
#include "function.h"
#include "scope.h"
#include "value.h"

#include <iostream>
#include <sstream>
#include <vector>

#ifndef CEOS_VM_H
#define CEOS_VM_H

namespace ceos {

  class VM {
    public:
      VM(std::stringstream &bs):
        stack(2048),
        pc(0) {
        std::string bc = bs.str();
        length = bc.length();
        m_bytecode = (uint8_t *)malloc(length);
        memcpy(m_bytecode, bc.data(), length);
        m_scope = std::make_shared<Scope>(nullptr);
        m_scope->parent = m_scope; // global scope
        registerBuiltins();
      }

      void registerBuiltins();
      void execute();
      void loadStrings();
      void loadFunctions();
      void run();

      void stack_push(Value value) {
        if (esp > stack.size() - 2) {
          std::cerr << "Stack overflow\n";
          throw;
        }
        stack[esp++] = value;
      }

      Value stack_pop() {
        return stack[--esp];
      }

      Value arg(unsigned index) {
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

      std::vector<Value> stack;
      unsigned ebp;
      unsigned esp;
      unsigned pc;
      size_t length;

      std::vector<std::string> m_stringTable;

    private:
      uint8_t *m_bytecode;
      std::vector<Function> m_userFunctions;
      std::shared_ptr<Scope> m_scope;
  };

}

#endif
