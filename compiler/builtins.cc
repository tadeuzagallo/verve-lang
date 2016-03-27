#include "value.h"
#include "vm.h"

#include <cassert>

namespace ceos {

  void registerBuiltins(VM &vm) {

#define REGISTER(NAME, FN) Builtin FN##_ = FN; vm.m_scope->set(#NAME, Value(FN##_))

    REGISTER(print, print);
    REGISTER(list, list);

    REGISTER(add, add);
    REGISTER(sub, sub);
    REGISTER(mul, mul);
    REGISTER(div, div);
    REGISTER(lt, lt);
    REGISTER(gt, gt);
    REGISTER(lte, lte);
    REGISTER(gte, gte);
    REGISTER(equals, equals);
    REGISTER(and, _and);
    REGISTER(or, _or);
    REGISTER(at, at);
    REGISTER(substr, substr);
    REGISTER(count, count);
    REGISTER(__heap-size__, heapSize);
  }


#define EACH_ARG(IT) \
  Value IT; for (unsigned I = 0; (I < argv ? (IT = vm.arg(I)) : 0), I < argv; I++)

#define BASIC_MATH(NAME, OP) \
  JS_FUNCTION(NAME) { \
    assert(argv == 2); \
 \
    return Value(vm.arg(0).asInt() OP vm.arg(1).asInt()); \
  }

  BASIC_MATH(add, +)
  BASIC_MATH(sub, -)
  BASIC_MATH(mul, *)
  BASIC_MATH(div, /)
  BASIC_MATH(lt, <)
  BASIC_MATH(gt, >)
  BASIC_MATH(lte, <=)
  BASIC_MATH(gte, >=)
  BASIC_MATH(equals, ==)
  BASIC_MATH(_and, &&)
  BASIC_MATH(_or, ||)

  JS_FUNCTION(print) {
    for (unsigned i = 0; i < argv; i++) {
      Value arg = vm.arg(i);
      if (arg.isString()) {
        std::cout << arg.asString()->c_str();
      } else if (arg.isArray()) {
        for (auto a : *arg.asArray()) {
          std::cout << a.asInt() << " ";
        }
      } else {
        std::cout << arg.asInt();
      }

      if (i < argv - 1) {
        std::cout << " ";
      }
    }
    std::cout << "\n";

    return 0;
  }

  JS_FUNCTION(at) {
    assert(argv == 2);

    Value arg = vm.arg(0);
    if (arg.isString()) {
      return arg.asString()->c_str()[vm.arg(1).asInt()];
    } else if (arg.isArray()) {
      auto array = arg.asArray();
      return array->at(vm.arg(1).asInt());
    }

    return Value(0);
  }

  JS_FUNCTION(substr) {
    assert(argv == 2 || argv == 3);

    Value arg = vm.arg(0);
    if (arg.isString()) {
      std::string *str = arg.asString();
      std::string substring;
      if (argv == 2) {
        substring = str->substr(vm.arg(1).asInt());
      } else {
        substring = str->substr(vm.arg(1).asInt(), vm.arg(2).asInt());
      }
      auto s = new std::string(substring);
      vm.trackAllocation(s, sizeof(std::string) + s->capacity());
      return Value(s);
    } else {
      throw;
    }

    return 0;
  }

  JS_FUNCTION(count) {
    assert(argv == 1);

    Value arg = vm.arg(0);
    if (arg.isString()) {
      return arg.asString()->length();
    } else {
      throw;
    }

    return 0;
  }

  JS_FUNCTION(list) {
    auto list = new std::vector<Value>();
    EACH_ARG(arg) {
      list->push_back(arg);
    }
    vm.trackAllocation(list, sizeof(std::vector<Value>) + list->capacity() * sizeof(Value));
    return Value(list);
  }

  JS_FUNCTION(heapSize) {
    assert(argv == 0);

    return Value((int)vm.heapSize);
  }

}
