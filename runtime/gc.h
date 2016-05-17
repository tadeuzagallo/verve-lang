#include "value.h"
#include "scope.h"
#include "closure.h"

#include <set>
#include <vector>

#pragma once

namespace ceos {

  typedef std::vector<std::pair<size_t, void *>> Heap;

  class GC {
    public:
      static void start() {
        roots.clear();
        scopes.clear();
      }

      static void markValue(Value &value, Heap &heap) {
        if (!value.isHeapAllocated()) {
          return;
        }

        if (roots.find(value.encode()) != roots.end()) {
          return;
        }

        roots.insert(value.encode());

        auto ptr = value.asPtr();
        auto it = heap.begin();

        while (it != heap.end()) {
          if (it->second == ptr) break;
          else ++it;
        }

        if (it != heap.end()) {
          it->second = mark(it->second);

          if (value.isArray()) {
            for (auto i : *value.asArray()) {
              markValue(i, heap);
            }
          } else if (value.isClosure()) {
            Scope *scope;
            if ((scope = value.asClosure()->scope) != NULL) {
              markScope(scope, heap);
            }
          }
        }
      }

      static void markScope(Scope *scope, Heap &heap) {
        if (scopes.find(scope) != scopes.end()) {
          return;
        }

        scopes.insert(scope);

        scope->visit([&heap](Value value) {
            markValue(value, heap);
        });

        if (scope->parent) {
          markScope(scope->parent, heap);
        }
      }

      static void sweep(Heap &heap, size_t *heapSize) {
        auto it = heap.begin();
        while (it != heap.end()) {
          if (GC::isMarked(it->second)) {
            it->second = GC::unmark(it->second);
            ++it;
          } else {
            free(it->second);
            *heapSize -= it->first;
            it = heap.erase(it);
          }
        }

      }

      static void *mark(void *ptr) {
        return reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(ptr) | 1);
      }

      static bool isMarked(void *ptr) {
        return reinterpret_cast<uintptr_t>(ptr) & 1;
      }

      static void *unmark(void *ptr) {
        return reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(ptr) & (~1));
      }
    private:

      static std::set<uint64_t> roots;
      static std::set<Scope *> scopes;
  };
}
