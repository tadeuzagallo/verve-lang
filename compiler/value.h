#include <cstdint>
#include <string>
#include <vector>

#ifndef CEOS_VALUE_H
#define CEOS_VALUE_H

namespace ceos {
  class VM;
  struct Closure;

  struct Value {
    union {
      uint64_t raw;
      uintptr_t ptr;
      struct {
        int32_t i;
        uint16_t __;
        uint8_t _;
        uint8_t tag;
      } data;
    } value;

#define TAG(NAME, VALUE) \
    static const uint8_t NAME##Tag = VALUE; \
    bool is##NAME() { return value.data.tag == Value::NAME##Tag; }

    TAG(Int, 0); // fast path from assembly
    TAG(Undefined, 1 << 0);
    TAG(String,    1 << 1);
    TAG(Array,     1 << 2);
    TAG(Builtin,   1 << 3);
    TAG(Closure,   1 << 4);

#undef TAG

    static uintptr_t unmask(uintptr_t ptr) {
      return 0xFFFFFFFFFFFFFF & ptr;
    }

    Value() {
      value.ptr = 0;
      value.data.tag = Value::UndefinedTag;
    }

    Value(int v) {
      value.data.i = v;
      value.data.tag = Value::IntTag;
    }

    int asInt() { return value.data.i; }

#define POINTER_TYPE(TYPE, NAME) \
    Value(TYPE *ptr) { \
      value.ptr = reinterpret_cast<uintptr_t>(ptr); \
      value.data.tag = Value::NAME##Tag; \
    }\
    \
    TYPE *as##NAME() { \
      return reinterpret_cast<TYPE *>(unmask(value.ptr)); \
    }

    POINTER_TYPE(char, String)
    POINTER_TYPE(std::vector<Value>, Array)
    POINTER_TYPE(Closure, Closure)

#undef POINTER_TYPE

    typedef Value (*Builtin)(unsigned, Value *, VM *);

    Value(Builtin ptr) {
      value.ptr = reinterpret_cast<uintptr_t>(ptr);
      value.data.tag = Value::BuiltinTag;
    }

    Builtin asBuiltin() {
      return reinterpret_cast<Builtin>(unmask(value.ptr));
    }

    void *asPtr() {
      return reinterpret_cast<void *>(unmask(value.ptr));
    }

    bool isHeapAllocated() {
      return value.data.tag & (Value::ClosureTag | Value::ArrayTag | Value::StringTag);
    }

    uint64_t encode() {
      return value.raw;
    }

    static Value decode(uint64_t data) {
      Value v;
      v.value.raw = data;
      return v;
    }
  };

}

using Builtin = ceos::Value::Builtin;

#endif
