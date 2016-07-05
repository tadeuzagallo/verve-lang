#include "utils/macros.h"

#pragma once

#define WORD_SIZE 8

#define OPCODE_ADDRESS(__op, _) (uintptr_t)op_##__op,

#define EXTERN_OPCODE(opcode, _) \
  extern "C" void op_##opcode ();

#define OPCODES \
      ret, 0, \
      bind, 1, \
      push, 1, \
      call, 1, \
      jz, 1, \
      jmp, 1, \
      create_closure, 2, \
      load_string, 1, \
      push_arg, 1, \
      lookup, 2, \
      exit, 0, \
      create_lex_scope, 0, \
      release_lex_scope, 0, \
      put_to_scope, 1, \
      alloc_obj, 2, \
      alloc_list, 1, \
      obj_store_at, 1, \
      obj_tag_test, 1, \
      obj_load, 1, \
      stack_alloc, 1, \
      stack_store, 1, \
      stack_load, 1, \
      stack_free, 1

EVAL(MAP_2(EXTERN_OPCODE, OPCODES))

#define FIRST_WITH_COMMA(F, ...) F,
#define SECOND_WITH_COMMA(_, S, ...) S,

namespace Verve {

class Opcode {
  public:

  EVAL(ENUM(Type, MAP_2(FIRST_WITH_COMMA, OPCODES)));

  static uintptr_t address(Opcode::Type t) {
    return (uintptr_t []) {
      EVAL(MAP_2(OPCODE_ADDRESS, OPCODES))
    }[(int)t];
  }

  static unsigned size(Opcode::Type t) {
    return (unsigned []) {
      EVAL(MAP_2(SECOND_WITH_COMMA, OPCODES))
    }[(int)t];
  }
};

}
