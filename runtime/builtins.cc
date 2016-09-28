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

    REGISTER(print_string, print_string);
    REGISTER(concat_string, concat_string);

    REGISTER(head, head);
    REGISTER(tail, tail);
    REGISTER(length, length);

    // type conversion
    REGISTER(int_to_string, int_to_string);
    REGISTER(float_to_string, float_to_string);

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
    REGISTER(!=, not_equal);
    REGISTER(&&, _and);
    REGISTER(||, _or);

    REGISTER(unary_!, _not);
    REGISTER(unary_-, minus);

    REGISTER(at, at);
    REGISTER(substr, substr);
    REGISTER(count, count);
    REGISTER(__heap-size__, heapSize);

    REGISTER(type$map, type_map);
  }


#define EACH_ARG(IT) \
  Value IT; for (unsigned I = 0; (I < argc ? (IT = argv[I]) : 0), I < argc; I++)

#define BASIC_MATH(NAME, OP) \
  VERVE_FUNCTION(NAME) { \
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
  BASIC_MATH(not_equal, !=)
  BASIC_MATH(_and, &&)
  BASIC_MATH(_or, ||)

  VERVE_FUNCTION(_not) {
    assert(argc == 1);
    return Value(!argv[0].asInt());
  }

  VERVE_FUNCTION(minus) {
    assert(argc == 1);
    return Value(-argv[0].asInt());
  }

  VERVE_FUNCTION(print_string) {
    assert(argc == 1);

    printf("%s", argv[0].asString().str());
    putchar('\n');

    return 0;
  }

  VERVE_FUNCTION(head) {
    assert(argc == 1);

    auto lst = argv[0].asList();
    if (lst->length  == 0) 
      return Value{};
    else
      return lst->at(0);
  }

  VERVE_FUNCTION(tail) {
    assert(argc == 1);

    auto lst = argv[0].asList();
    auto size = lst->length > 0 ? lst->length - 1 : 0;
    auto ret = (uint64_t *)calloc(size + 1, 8);
    ret[0] = size;
    for (unsigned i = 1; i < lst->length; i++) {
      ret[i] = lst->at(i).encode();
    }
    vm->trackAllocation(ret, (size + 1) * 8);
    return (List *)ret;
  }

  VERVE_FUNCTION(length) {
    assert(argc == 1);

    return argv[0].asList()->length;
  }

  VERVE_FUNCTION(int_to_string) {
    assert(argc == 1);

    auto number = argv[0].asInt();
    auto size = snprintf(NULL, 0, "%d", number);
    auto buffer = (char *)malloc(size + 1);
    snprintf(buffer, size + 1, "%d", number);
    vm->trackAllocation(buffer, size + 1);

    return Value(buffer);
  }

  VERVE_FUNCTION(float_to_string) {
    assert(argc == 1);

    auto v = argv[0].encode();
    auto number = *(double *)&v;
    auto size = snprintf(NULL, 0, "%lg", number);
    auto buffer = (char *)malloc(size + 1);
    snprintf(buffer, size + 1, "%lg", number);
    vm->trackAllocation(buffer, size + 1);

    return Value(buffer);
  }

  VERVE_FUNCTION(concat_string) {
    assert(argc == 2);

    auto s1 = argv[0].asString().str();
    auto s2 = argv[1].asString().str();
    auto size = strlen(s1) + strlen(s2);
    auto buffer = (char *)malloc(size + 1);
    snprintf(buffer, size + 1, "%s%s", s1, s2);
    vm->trackAllocation(buffer, size + 1);

    return Value(buffer);
  }

  VERVE_FUNCTION(at) {
    assert(argc == 2);

    Value arg = argv[0];
    if (arg.isString()) {
      return arg.asString()[argv[1].asInt()];
    } else if (arg.isList()) {
      return arg.asList()->at(argv[1].asInt());
    }

    return Value(0);
  }

  VERVE_FUNCTION(substr) {
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

  VERVE_FUNCTION(count) {
    assert(argc == 1);

    Value arg = argv[0];
    if (arg.isString()) {
      return strlen(arg.asString());
    } else {
      throw;
    }

    return 0;
  }

  VERVE_FUNCTION(heapSize) {
    assert(argc == 0);

    return Value((int)vm->heapSize);
  }

  VERVE_FUNCTION(type_map) {
    assert(argc == 2);

    auto dictName = argv[0].asString();
    auto typeIndex = argv[1].asInt();

    auto typeMap = vm->m_typeMaps[dictName.str()];
    auto fnNameIndex = typeMap[typeIndex];
    auto fnName = vm->m_stringTable[fnNameIndex];

    return vm->m_scope->get(fnName);
  }
}
