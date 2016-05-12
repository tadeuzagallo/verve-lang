#include <cstdint>
#include <string>
#include <vector>

#include "ceos_string.h"
#include "macros.h"

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
    ALWAYS_INLINE bool is##NAME() { return value.data.tag == Value::NAME##Tag; }

    TAG(Int, 0); // fast path from assembly
    TAG(Undefined, 1 << 0);
    TAG(String,    1 << 1);
    TAG(Array,     1 << 2);
    TAG(Builtin,   1 << 3);
    TAG(Closure,   1 << 4);

#undef TAG

    ALWAYS_INLINE static uintptr_t unmask(uintptr_t ptr) {
      return 0xFFFFFFFFFFFFFF & ptr;
    }

    ALWAYS_INLINE Value() {
      value.ptr = 0;
      value.data.tag = Value::UndefinedTag;
    }

    ALWAYS_INLINE Value(int v) {
      value.raw = v;
    }

    ALWAYS_INLINE int asInt() { return value.data.i; }

#define POINTER_TYPE(TYPE, NAME) \
    ALWAYS_INLINE Value(TYPE *ptr) { \
      value.ptr = reinterpret_cast<uintptr_t>(ptr); \
      value.data.tag = Value::NAME##Tag; \
    }\
    \
    ALWAYS_INLINE TYPE *as##NAME() { \
      return reinterpret_cast<TYPE *>(unmask(value.ptr)); \
    }

    POINTER_TYPE(std::vector<Value>, Array)
    POINTER_TYPE(Closure, Closure)

    ALWAYS_INLINE Value(String str) {
      value.ptr = reinterpret_cast<uintptr_t>(str.str());
      value.data.tag = Value::StringTag;
    }

    ALWAYS_INLINE String asString() {
      return String(reinterpret_cast<char *>(unmask(value.ptr)));
    }

#undef POINTER_TYPE

    typedef Value (*Builtin)(unsigned, Value *, VM *);

    ALWAYS_INLINE Value(Builtin ptr) {
      value.ptr = reinterpret_cast<uintptr_t>(ptr);
      value.data.tag = Value::BuiltinTag;
    }

    ALWAYS_INLINE Builtin asBuiltin() {
      return reinterpret_cast<Builtin>(unmask(value.ptr));
    }

    ALWAYS_INLINE void *asPtr() {
      return reinterpret_cast<void *>(unmask(value.ptr));
    }

    ALWAYS_INLINE bool isHeapAllocated() {
      return value.data.tag & (Value::ClosureTag | Value::ArrayTag | Value::StringTag);
    }

    ALWAYS_INLINE uint64_t encode() {
      return value.raw;
    }

    ALWAYS_INLINE static Value decode(uint64_t data) {
      Value v;
      v.value.raw = data;
      return v;
    }

    ALWAYS_INLINE static Value fastClosure(unsigned offset) {
      Value v;
      v.value.data.i = (offset << 1) | 1;
      v.value.data.tag = ClosureTag;
      return v;
    }
  };

}

using Builtin = ceos::Value::Builtin;

#endif
