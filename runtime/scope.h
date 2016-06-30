#include "verve_string.h"
#include "value.h"

#include <cassert>
#include <cstdlib>
#include <functional>

#pragma once

#define DEFAULT_SIZE 8

namespace verve {
  class ScopeTest;

  struct Scope {

    friend class ScopeTest;

    Scope(unsigned size = 0) {
      assert(size % 2 == 0);
      refCount = 1;
      length = 0;
      tableSize = 0;
      table = NULL;
      parent = NULL;
      previous = NULL;

      if (size) {
        resize(size);
      }
    }

    void resize(unsigned size) {
      Entry *oldTable = nullptr;
      size_t oldSize = 0;
      if (size) {
        oldSize = tableSize;
        tableSize = size;
        oldTable = table;
      } else {
        tableSize = DEFAULT_SIZE;
      }

      tableHash = tableSize - 1;
      table = (Entry *)calloc(tableSize, sizeof(Entry));

      if (oldTable) {
        for (unsigned i = 0; i < oldSize; i++) {
          set(oldTable[i].key, oldTable[i].value);
        }
        free(oldTable);
      }
    }

    Scope *inc() {
      refCount++;
      return this;
    }

    void dec() {
      assert(refCount > 0);
      if (!--refCount) {
        if (parent) { parent->dec(); parent = NULL; }
        if (previous) { previous->dec(); previous = NULL; }

        if (s_scopePoolIndex == s_scopePoolSize) {
          s_scopePoolSize = s_scopePoolSize ? s_scopePoolSize << 1 : DEFAULT_SIZE << 1;
          s_scopePool = (Scope **)realloc(s_scopePool, s_scopePoolSize * sizeof(Scope *));
        }
        s_scopePool[s_scopePoolIndex++] = this;
      }
    }

    static inline Scope *getScope() {
      if (s_scopePoolIndex <= 0) {
        return new Scope();
      }

      auto s = s_scopePool[--s_scopePoolIndex];
      s->refCount = 1;
      s->length = 0;
      memset(s->table, 0, s->tableSize * sizeof(Entry));
      return s;
    }

    Scope *create(Scope *p) {
      auto s = getScope();
      s->parent = p->inc();
      s->previous = this->inc();
      return s;
    }

    Scope *create() {
      auto s = getScope();
      s->parent = this->inc();
      return s;
    }

    Scope *restore() {
      auto ret = previous ?: parent ?: this;
      if (ret != this) this->dec();
      return ret;
    }

    Value get(String key) {
      if (tableSize) {
        unsigned index = reinterpret_cast<uintptr_t>(key.str()) & tableHash;
        auto begin = index;
        while (table[index].key != NULL) {
          if (table[index].key == key) {
            return table[index].value;
          }
          if ((index = (index + 1) & tableHash) == begin) break;
        }
      }
      if (parent) return parent->get(key);
      return Value();
    }

    void set(String key, Value value) {
      if (length == tableSize) {
        resize(tableSize << 1);
      }

      unsigned index = reinterpret_cast<uintptr_t>(key.str()) & tableHash;
      auto begin = index;
      do {
        if (table[index].key == NULL || table[index].key == key) {
          if (table[index].key != key) length++;

          table[index].key = key;
          table[index].value = value;
          return;
        }
      } while((index = (index + 1) & tableHash) != begin);
      throw;
    }

    void visit(std::function<void(Value)> visitor) {
      for (unsigned i = 0; i < tableSize; i++) {
        if (table[i].key != 0) {
          visitor(table[i].value);
        }
      }
    }

    struct Entry {
      String key;
      Value value;
    };


    Entry *table;
    Scope *parent;
    Scope *previous;
    unsigned tableHash;
  private:
    unsigned refCount;
    unsigned length;
    unsigned tableSize;

    static Scope **s_scopePool;
    static unsigned s_scopePoolIndex;
    static unsigned s_scopePoolSize;
  };

}
