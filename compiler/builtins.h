#ifndef CEOS_BUILTINS_H
#define CEOS_BUILTINS_H

#define JS_FUNCTION(FN_NAME) Value FN_NAME(ceos::VM &vm, unsigned argv)

namespace ceos {
  class VM;
  struct Value;

  JS_FUNCTION(print);
  JS_FUNCTION(list);
  JS_FUNCTION(add);
  JS_FUNCTION(sub);
  JS_FUNCTION(mul);
  JS_FUNCTION(div);
  JS_FUNCTION(lt);
  JS_FUNCTION(gt);
  JS_FUNCTION(lte);
  JS_FUNCTION(gte);
  JS_FUNCTION(equals);
  JS_FUNCTION(_and);
  JS_FUNCTION(_or);
  JS_FUNCTION(at);
  JS_FUNCTION(substr);
  JS_FUNCTION(count);
  JS_FUNCTION(heapSize);

  void registerBuiltins(VM &);

}

#endif
