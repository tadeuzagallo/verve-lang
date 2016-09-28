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
#include <unordered_map>

#pragma once

namespace Verve {

  class VM {
    public:
      VM(uint8_t *bytecode, size_t len, bool needsLinking = false):
        m_scope(new Scope(32)),
        pc(0),
        length(len),
        heapSize(0),
        heapLimit(10240),
        m_needsLinking(needsLinking),
        m_bytecode(bytecode)
      {
        registerBuiltins(*this);
      }

      void execute();
      void linkBytecode();
      inline void loadStrings();
      inline void loadTypeMaps();
      inline void loadFunctions();
      inline void loadText();
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

      bool m_needsLinking;
      std::vector<String> m_stringTable;
      std::vector<Function> m_userFunctions;
      std::unordered_map<std::string, std::vector<int>> m_typeMaps;

    private:
      uint8_t *m_bytecode;
  };
}
