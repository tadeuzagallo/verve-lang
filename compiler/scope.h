#include "ceos_string.h"
#include "value.h"

#include <cassert>
#include <cstdlib>
#include <functional>

#ifndef CEOS_SCOPE_H
#define CEOS_SCOPE_H

#define DEFAULT_SIZE 4

namespace ceos {
  class ScopeTest;

  struct Scope {

    friend class ScopeTest;

    Scope(unsigned size = DEFAULT_SIZE) {
      refCount = 1;
      length = 0;
      tableSize = 0;
      table = NULL;
      parent = NULL;
      previous = NULL;

      resize(size);
    }

    void resize(unsigned size) {
      //assert(size > 0);
      unsigned i = tableSize;
      tableSize = size;
      table = (Entry *)realloc(table, sizeof(Entry) * size);
      while (i < size) {
        table[i++].key = NULL;
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
          s_scopePoolSize = s_scopePoolSize ? s_scopePoolSize << 1 : DEFAULT_SIZE;
          s_scopePool = (Scope **)realloc(s_scopePool, s_scopePoolSize * sizeof(Scope *));
        }
        s_scopePool[s_scopePoolIndex++] = this;
      }
    }

    static inline Scope *getScope(unsigned size = DEFAULT_SIZE) {
      if (s_scopePoolIndex <= 0) {
        return new Scope(size);
      }

      auto s = s_scopePool[--s_scopePoolIndex];
      unsigned prevSize = s->tableSize;
      s->refCount = 1;
      s->length = 0;
      s->tableSize = 0;
      s->resize(std::max(prevSize, size));
      return s;
    }

    Scope *create(Scope *p, unsigned size = DEFAULT_SIZE) {
      auto s = getScope(size);
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
      unsigned index = reinterpret_cast<uintptr_t>(key.str()) % tableSize;
      auto begin = index;
      while (table[index].key != NULL) {
        if (table[index].key == key) {
          return table[index].value;
        }
        if ((index = (index + 1) % tableSize) == begin) break;
      }
      if (parent) return parent->get(key);
      return Value();
    }

    void set(String key, Value value) {
      if (length == tableSize) {
        resize(tableSize << 1);
      }

      unsigned index = reinterpret_cast<uintptr_t>(key.str()) % tableSize;
      auto begin = index;
      do {
        if (table[index].key == NULL || table[index].key == key) {
          if (table[index].key != key) length++;

          table[index].key = key;
          table[index].value = value;
          return;
        }
      } while((index = (index + 1) % tableSize) != begin);
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

    Scope *parent;

  private:
    Entry *table;
    Scope *previous;
    unsigned refCount;
    unsigned length;
    unsigned tableSize;

    static Scope **s_scopePool;
    static unsigned s_scopePoolIndex;
    static unsigned s_scopePoolSize;
  };

}

#endif
