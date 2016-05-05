#include "builtins.h"
#include "closure.h"
#include "gc.h"
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
        pc(0),
        heapLimit(10240) {

        std::string bc = bs.str();
        length = bc.length();
        m_bytecode = (uint8_t *)malloc(length);
        memcpy(m_bytecode, bc.data(), length);

        m_scope = std::make_shared<Scope<Value>>();

        registerBuiltins(*this);
      }

      void execute();
      void inflate();
      void loadStrings();
      void loadFunctions();
      void run();
      void trackAllocation(void *, size_t);
      void collect();

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
      size_t heapSize;
      size_t heapLimit;
      std::vector<std::pair<size_t, void *>> blocks;

      std::shared_ptr<Scope<Value>> m_scope;
      std::vector<char *> m_stringTable;
      std::vector<Function> m_userFunctions;

    private:
      uint8_t *m_bytecode;
  };
}

#endif
