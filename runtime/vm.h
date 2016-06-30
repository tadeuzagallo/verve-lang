#include "builtins.h"
#include "verve_string.h"
#include "closure.h"
#include "gc.h"
#include "function.h"
#include "scope.h"
#include "value.h"

#include <iostream>
#include <sstream>
#include <vector>

#pragma once

namespace Verve {

  class VM {
    public:
      VM(std::stringstream &bs):
        pc(0),
        heapLimit(10240) {

        std::string bc = bs.str();
        length = bc.length();
        m_bytecode = (uint8_t *)malloc(length);
        memcpy(m_bytecode, bc.data(), length);

        m_scope = new Scope(32);
        registerBuiltins(*this);
      }

      void execute();
      void inflate();
      inline void loadStrings();
      inline void loadFunctions();
      void trackAllocation(void *, size_t);
      void collect();

      template<typename T>
      inline T read() {
        T v = *(T *)(m_bytecode + pc);
        pc += sizeof(T);
        return v;
      }

      String readStr() {
        char *v = (char *)(m_bytecode + pc);
        pc += strlen(v) + 1;
        return String(v);
      }

      Scope *m_scope; // first thing, easy to access from asm

      unsigned pc;
      size_t length;
      size_t heapSize;
      size_t heapLimit;
      std::vector<std::pair<size_t, void *>> blocks;

      std::vector<String> m_stringTable;
      std::vector<Function> m_userFunctions;

    private:
      uint8_t *m_bytecode;
  };
}
