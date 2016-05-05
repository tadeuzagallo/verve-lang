#include "scope.h"
#include "closure.h"

#include <stdio.h>
#include <stdlib.h>

#define assert(COND) \
if (!(COND)) { \
  fprintf(stderr, "Assertion failed on line %d: %s\n", __LINE__, #COND); \
  exit(10); \
}

namespace ceos {


class ScopeTest {
  public:

  static void testScopeCreate() {
    Scope *global = new Scope();
    auto tmp = global->create();
    tmp->restore();
    assert(tmp->refCount == 0);
  }

  static void testClosure() {
    {
      // parent == previous
      auto global = new Scope();
      auto closure = new Closure(global);
      auto tmp = global->create(closure->scope);
      tmp->restore();
      assert(tmp->refCount == 0);
      assert(global->refCount == 2);
      delete closure;
      assert(global->refCount == 1);
    }

    {
      // parent != previous
      auto global = new Scope();
      auto tmp = global->create();
      auto closure = new Closure(global);
      auto tmp2 = tmp->create(closure->scope);
      assert(tmp2->refCount == 1);
      assert(tmp->refCount == 2);
      assert(global->refCount == 4);

      tmp2->restore();
      assert(tmp2->refCount == 0);
      assert(tmp->refCount == 1);
      assert(global->refCount == 3);

      tmp->restore();
      assert(tmp->refCount == 0);
      assert(global->refCount == 2);

      delete closure;
      assert(global->refCount == 1);
    }
  }

  static void test() {
    testScopeCreate();
    testClosure();
  }

};

}

int main() {
  ceos::ScopeTest::test();
  return 0;
}
