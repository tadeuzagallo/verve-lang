#include "value.h"

#include <cstdlib>
#include <functional>

#ifndef CEOS_SCOPE_H
#define CEOS_SCOPE_H

#define DEFAULT_SIZE 3

namespace ceos {
  class ScopeTest;

  namespace {
    static inline unsigned hash(const char *str) {
      unsigned long hash = 5381;
      int c;
      while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
      }
      return hash;
    }
  }

  struct Scope {

    friend class ScopeTest;

    Scope(unsigned size = DEFAULT_SIZE) {
      refCount = 1;
      length = 0;
      table = NULL;
      parent = NULL;
      previous = NULL;

      resize(size);
    }

    void resize(unsigned size) {
      //assert(size > 0);
      tableSize = size;
      if (table) free(table);
      table = (Entry *)calloc(sizeof(Entry), size);
    }

    ~Scope() {
      if (parent) parent->dec();
      if (previous) previous->dec();
    }

    Scope *inc() {
      refCount++;
      return this;
    }

    void dec() {
      if (!--refCount) {
        delete this;
      }
    }

    Scope *create(Scope *p, unsigned size = DEFAULT_SIZE) {
      auto s = new Scope(size);
      s->parent = p->inc();
      s->previous = this->inc();
      return s;
    }

    Scope *create() {
      auto s = new Scope();
      s->parent = this->inc();
      return s;
    }

    Scope *restore() {
      auto ret = previous ?: parent ?: this;
      if (ret != this) this->dec();
      return ret;
    }

    Value get(char *key) {
      unsigned hash = ::ceos::hash(key);
      unsigned index = hash % tableSize;
      auto begin = index;
      while (table[index].key != 0) {
        if (table[index].key == hash) {
          return table[index].value;
        } 
        if ((index = (index + 1) % tableSize) == begin) break;
      }
      if (parent) return parent->get(key);
      return Value();
    }

    void set(char *key, Value value) {
      if (length == tableSize) {
        resize(tableSize * 2); 
      }

      unsigned hash = ::ceos::hash(key);
      unsigned index = hash % tableSize;
      auto begin = index;
      do {
        if (table[index].key == 0 || table[index].key == hash) {
          if (table[index].key != hash) length++;

          table[index].key = hash;
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
      unsigned key;
      Value value;
    };

    Scope *parent;

  private:
    Entry *table;
    Scope *previous;
    unsigned refCount;
    unsigned length;
    unsigned tableSize;
  };

}

#endif
