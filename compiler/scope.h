#include "value.h"

#include <functional>

#ifndef CEOS_SCOPE_H
#define CEOS_SCOPE_H

namespace ceos {
  class ScopeTest;

  namespace {
    static inline unsigned hash(const char *str) {
      unsigned limit = 16;
      unsigned long hash = 5381;
      int c;
      while ((c = *str++) && --limit) {
        hash = ((hash << 5) + hash) + c;
      }
      return hash;
    }
  }

  struct Scope {

    friend class ScopeTest;

    Scope() : Scope(32) {}

    Scope(unsigned size) : cacheSize(size), cacheHash(size - 1) {
      refCount = 1;
      table = new Entry[cacheSize]();
      parent = NULL;
      previous = NULL;
    }

    ~Scope() {
      if (parent) parent->dec();
      if (previous) previous->dec();
    }

    inline Scope *inc() {
      refCount++;
      return this;
    }
    inline void dec() {
      if (!--refCount) {
        delete this;
      }
    }

    inline Scope *create(Scope *p) {
      auto s = new Scope();
      s->parent = p->inc();
      s->previous = this->inc();
      return s;
    }

    inline Scope *create() {
      auto s = new Scope();
      s->parent = this->inc();
      s->previous = NULL;
      return s;
    }

    inline Scope *restore() {
      auto ret = previous ?: parent ?: this;
      if (ret != this) this->dec();
      return ret;
    }

    Value get(char *key) {
      unsigned index = hash(key) % cacheSize;
      auto begin = index;
      while (table[index].key != NULL) {
        if (table[index].key == key || strcmp(table[index].key, key) == 0) {
          return table[index].value;
        } 
        if ((index = (index + 1) & cacheHash) == begin) break;
      }
      if (parent) return parent->get(key);
      return Value();
    }

    void set(char *key, Value value) {
      unsigned index = hash(key) % cacheSize;
      auto begin = index;
      do {
        if (table[index].key == NULL || table[index].key == key) {
          table[index].key = key;
          table[index].value = value;
          return;
        }
      } while((index = (index + 1) & cacheHash) != begin);
      throw;
    }

    inline void visit(std::function<void(Value)> visitor) {
      for (unsigned i = 0; i < cacheSize; i++) {
        if (table[i].key != NULL) {
          visitor(table[i].value);
        }
      }
    }

    struct Entry {
      char *key;
      Value value;
    };

    Scope *parent;

  private:
    Entry *table;
    Scope *previous;
    unsigned refCount;
    unsigned cacheSize;
    unsigned cacheHash;
  };

}

#endif
