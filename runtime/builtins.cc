#include "value.h"
#include "vm.h"

#include <cassert>

extern "C" void *builtin_sub();
extern "C" void *builtin_add();
extern "C" void *builtin_lt();

namespace Verve {

  void registerBuiltins(VM &vm) {

#define REGISTER(NAME, FN) \
    do { \
      Builtin FN##_ = (Builtin)FN;  \
      vm.m_scope->set(#NAME, Value(FN##_)); \
    } while(0)

    REGISTER(print, print);

    REGISTER(+, builtin_add);
    REGISTER(-, builtin_sub);
    REGISTER(*, mul);
    REGISTER(/, div);
    REGISTER(%, mod);
    REGISTER(<, builtin_lt);
    REGISTER(>, gt);
    REGISTER(<=, lte);
    REGISTER(>=, gte);
    REGISTER(==, equals);
    REGISTER(&&, _and);
    REGISTER(||, _or);

    REGISTER(unary_!, _not);
    REGISTER(unary_-, minus);

    REGISTER(at, at);
    REGISTER(substr, substr);
    REGISTER(count, count);
    REGISTER(__heap-size__, heapSize);
  }


#define EACH_ARG(IT) \
  Value IT; for (unsigned I = 0; (I < argc ? (IT = argv[I]) : 0), I < argc; I++)

#define BASIC_MATH(NAME, OP) \
  JS_FUNCTION(NAME) { \
    assert(argc == 2); \
 \
    return Value(argv[0].asInt() OP argv[1].asInt()); \
  }

  BASIC_MATH(add, +)
  BASIC_MATH(sub, -)
  BASIC_MATH(mul, *)
  BASIC_MATH(div, /)
  BASIC_MATH(mod, %)
  BASIC_MATH(lt, <)
  BASIC_MATH(gt, >)
  BASIC_MATH(lte, <=)
  BASIC_MATH(gte, >=)
  BASIC_MATH(equals, ==)
  BASIC_MATH(_and, &&)
  BASIC_MATH(_or, ||)

  JS_FUNCTION(_not) {
    assert(argc == 1);
    return Value(!argv[0].asInt());
  }

  JS_FUNCTION(minus) {
    assert(argc == 1);
    return Value(-argv[0].asInt());
  }


  static void printValue(Value value) {
    if (value.isString()) {
      printf("%s", value.asString().str());
    } else if (value.isList()) {
      for (unsigned i = 0; i < value.asList()->length; i++) {
        if (i) putchar(' ');
        printValue(value.asList()->at(i));
      }
    } else if (value.isInt()){
      printf("%d", value.asInt());
    } else {
      throw std::runtime_error("Trying to print unsupported type");
    }
  }

  JS_FUNCTION(print) {
    for (unsigned i = 0; i < argc; i++) {
      if (i) putchar(' ');
      printValue(argv[i]);
    }
    putchar('\n');

    return 0;
  }

  JS_FUNCTION(at) {
    assert(argc == 2);

    Value arg = argv[0];
    if (arg.isString()) {
      return arg.asString()[argv[1].asInt()];
    } else if (arg.isList()) {
      return arg.asList()->at(argv[1].asInt());
    }

    return Value(0);
  }

  JS_FUNCTION(substr) {
    assert(argc == 2 || argc == 3);

    Value arg = argv[0];
    if (arg.isString()) {
      const char *str = arg.asString().str();
      const char *substring;
      if (argc == 2) {
        substring = str + argv[1].asInt();
      } else {
        size_t start = argv[0].asInt();
        size_t length = argv[1].asInt() - start;
        char *substr = (char *)malloc(length + 1);
        memcpy(substr, str, length);
        substr[length] = 0;
        vm->trackAllocation(substr, length);
        substring = substr;
      }

      return Value(substring);
    } else {
      throw;
    }

    return 0;
  }

  JS_FUNCTION(count) {
    assert(argc == 1);

    Value arg = argv[0];
    if (arg.isString()) {
      return strlen(arg.asString());
    } else {
      throw;
    }

    return 0;
  }

  JS_FUNCTION(heapSize) {
    assert(argc == 0);

    return Value((int)vm->heapSize);
  }

}
