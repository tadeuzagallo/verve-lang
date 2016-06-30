#include "utils/macros.h"

#pragma once

#define WORD_SIZE 8

#define OPCODE_ADDRESS(__op) (uintptr_t)op_##__op,

#define EXTERN_OPCODE(opcode) \
  extern "C" void op_##opcode ();

#define OPCODES \
      ret, \
      bind, \
      push, \
      call, \
      jz, \
      jmp, \
      create_closure, \
      load_string, \
      push_arg, \
      lookup, \
      exit, \
      create_lex_scope, \
      release_lex_scope, \
      put_to_scope, \
      alloc_obj, \
      alloc_list, \
      obj_store_at, \
      obj_tag_test, \
      obj_load, \
      stack_alloc, \
      stack_store, \
      stack_load, \
      stack_free

EVAL(MAP(EXTERN_OPCODE, OPCODES))

namespace verve {

class Opcode {
  public:

  ENUM(Type, OPCODES);

  static uintptr_t opcodeAddress(Opcode::Type t) {
    return (uintptr_t []) {
      EVAL(MAP(OPCODE_ADDRESS, OPCODES))
    }[(int)t];
  }
};

}
