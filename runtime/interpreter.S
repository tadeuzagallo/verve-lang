#include "utils/macros.h"

#define STRING_TAG    1 << 1
#define LIST_TAG      1 << 2
#define CLOSURE_TAG   1 << 4
#define OBJECT_TAG    1 << 5

#define BYTECODE r12
#define SCOPE_VARS r13
#define VM r14
#define BCBASE r15
#define LOOKUP rbx

.macro READ off, to
.if \off == 1
  mov 0x8(%BYTECODE), \to
.elseif \off == 2
  mov 0x10(%BYTECODE), \to
.else
  hlt
.endif
.endmacro

.macro SKIP count
.if \count == 0
  add $0x8, %BYTECODE
.elseif \count == 1
  add $0x10, %BYTECODE
.elseif \count == 2
  add $0x18, %BYTECODE
.else
  hlt
.endif
  jmp *(%BYTECODE)
.endmacro

.macro UNMASK reg
  shl $8, \reg
  shr $8, \reg
.endmacro

.macro CCALL fn
  push %rbx
  mov %rsp, %rbx
  and $-0x10, %rsp
  call \fn
  mov %rbx, %rsp
  pop %rbx
.endmacro

.macro GET_ARG off, to
  mov 0x20(%rbp, \off, 8), \to
.endmacro

.globl SYMBOL(execute)
SYMBOL(execute):
  push %rbp
  push %BYTECODE
  push %VM
  push %BCBASE
  push %LOOKUP
  mov %rsp, %rbp
  mov %rdi, %BYTECODE
  mov %rsi, STRINGS(%rip)
  mov %rdx, %VM
  mov %rcx, %BCBASE
  mov %r8,  %LOOKUP
  jmp *(%BYTECODE)

.globl SYMBOL(op_exit)
SYMBOL(op_exit):
  mov %rbp, %rsp
  pop %LOOKUP
  pop %BCBASE
  pop %VM
  pop %BYTECODE
  pop %rbp
  ret

.globl SYMBOL(op_lookup)
SYMBOL(op_lookup):
_op_lookup_fast_path:
  READ 2, %rdi
  mov (%LOOKUP, %rdi, 8), %rsi // Cached address
  test %rsi, %rsi
  jz _op_lookup_slow_path
  push %rsi
  SKIP 2

.globl SYMBOL(op_push)
SYMBOL(op_push):
  READ 1, %rdi
  push %rdi
  SKIP 1

.globl SYMBOL(op_push_arg)
SYMBOL(op_push_arg):
  READ 1, %rdi
  GET_ARG %rdi, %rax
  push %rax
  SKIP 1

.globl SYMBOL(op_jz)
SYMBOL(op_jz):
  pop %rdi
  test %rdi, %rdi
  jz _jz
  SKIP 1
_jz:
  READ 1, %rdi
  add %rdi, %BYTECODE
  jmp *(%BYTECODE)

.globl SYMBOL(op_jmp)
SYMBOL(op_jmp):
  READ 1, %rdi
  add %rdi, %BYTECODE
  jmp *(%BYTECODE)

.globl SYMBOL(op_call)
SYMBOL(op_call):
  // pop the callee from the stack
  pop %rcx

  // setup args
  READ 1, %rdi //argc
  mov %rsp, %rsi // argv just lives in the stack
  mov %VM, %rdx

  // check tag
  rol $8, %rcx
  test $CLOSURE_TAG, %cl
  jnz _op_call_closure

_op_call_builtin:
  shr $8, %rcx
  push %rdi
  CCALL *%rcx
  pop %rdi
  lea (%rsp, %rdi, 8), %rsp
  push %rax
  SKIP 1

_op_call_closure:
  shr $8, %rcx
  push %BYTECODE
  push %rdi
  push %rcx
  push %rbp
  mov %rsp, %rbp

  test $1, %rcx
  jnz _op_call_fast_closure

_op_call_slow_closure:
  CCALL SYMBOL(prepareClosure)
  lea (%BCBASE, %rax, 1), %BYTECODE
  jmp *(%BYTECODE)

_op_call_fast_closure:
  shr $1, %ecx
  lea (%BCBASE, %rcx, 1), %BYTECODE
  jmp *(%BYTECODE)


.globl SYMBOL(op_load_string)
SYMBOL(op_load_string):
  READ 1, %rdi
  mov STRINGS(%rip), %rsi
  mov (%rsi, %rdi, 8), %rdi
  rol $8, %rdi
  mov $STRING_TAG, %dil
  ror $8, %rdi
  push %rdi
  SKIP 1

.globl SYMBOL(op_create_closure)
SYMBOL(op_create_closure):
  mov %VM, %rdi
  READ 1, %rsi
  READ 2, %rdx
  CCALL SYMBOL(createClosure)
  push %rax
  SKIP 2

.globl SYMBOL(op_bind)
SYMBOL(op_bind):
  mov %VM, %rdi
  READ 1, %rsi
  pop %rdx
  mov STRINGS(%rip), %rcx
  mov (%rcx, %rsi, 8), %rsi
  CCALL SYMBOL(setScope)
  SKIP 1

.globl SYMBOL(op_create_lex_scope)
SYMBOL(op_create_lex_scope):
  mov %VM, %rdi
  CCALL SYMBOL(pushScope)
  SKIP 0

.globl SYMBOL(op_release_lex_scope)
SYMBOL(op_release_lex_scope):
  mov %VM, %rdi
  CCALL SYMBOL(restoreScope)
  SKIP 0

.globl SYMBOL(op_put_to_scope)
SYMBOL(op_put_to_scope):
  mov %VM, %rdi
  READ 1, %rsi
  pop %rdx

  mov STRINGS(%rip), %rcx
  mov (%rcx, %rsi, 8), %rsi

  CCALL SYMBOL(setScope)
  SKIP 1

.globl SYMBOL(op_alloc_obj)
SYMBOL(op_alloc_obj):
  mov %VM, %rdi
  READ 1, %esi
  CCALL SYMBOL(allocate)
  READ 2, %esi // tag
  mov %esi, (%rax)
  READ 1, %esi // size
  dec %esi
  mov %esi, 0x4(%rax)
  rol $8, %rax
  mov $OBJECT_TAG, %al
  ror $8, %rax
  push %rax
  SKIP 2

.globl SYMBOL(op_alloc_list)
SYMBOL(op_alloc_list):
  mov %VM, %rdi
  READ 1, %rsi
  CCALL SYMBOL(allocate)
  READ 1, %rsi
  dec %rsi
  mov %rsi, (%rax)
  rol $8, %rax
  mov $LIST_TAG, %al
  ror $8, %rax
  push %rax
  SKIP 1

.globl SYMBOL(op_obj_store_at)
SYMBOL(op_obj_store_at):
  pop %rdi // value
  pop %rdx // object
  mov %rdx, %rcx
  UNMASK %rdx
  READ 1, %rsi // index
  mov %rdi, (%rdx, %rsi, 8)
  push %rcx
  SKIP 1

.globl SYMBOL(op_obj_tag_test)
SYMBOL(op_obj_tag_test):
  pop %rdi // object
  UNMASK %rdi
  READ 1, %rsi // expected tag
  mov (%rdi), %edi // object's tag
  cmp %edi, %esi
  je _op_obj_tag_test_ok
  CCALL SYMBOL(tagTestFailed)
_op_obj_tag_test_ok:
  SKIP 1

.globl SYMBOL(op_obj_load)
SYMBOL(op_obj_load):
  pop %rdi // object
  UNMASK %rdi
  READ 1, %rsi // offset
  mov 0x8(%rdi, %rsi, 8), %rdi // SKIP tag
  push %rdi
  SKIP 1

.globl SYMBOL(op_stack_alloc)
SYMBOL(op_stack_alloc):
  push %SCOPE_VARS
  READ 1, %rdi
  sub %rdi, %rsp
  mov %rsp, %SCOPE_VARS
  SKIP 1

.globl SYMBOL(op_stack_store)
SYMBOL(op_stack_store):
  READ 1, %rdi // slot - offset on stack
  pop %rsi
  mov %rsi, (%SCOPE_VARS, %rdi, 8)
  SKIP 1

.globl SYMBOL(op_stack_load)
SYMBOL(op_stack_load):
  READ 1, %rdi // slot - offset on stack
  mov (%SCOPE_VARS, %rdi, 8), %rdi
  push %rdi
  SKIP 1

.globl SYMBOL(op_stack_free)
SYMBOL(op_stack_free):
  pop %rdx
  mov %SCOPE_VARS, %rsp
  READ 1, %rdi
  add %rdi, %rsp
  pop %SCOPE_VARS
  push %rdx
  SKIP 1

.globl SYMBOL(op_ret)
SYMBOL(op_ret):
  pop %rax
  mov %rbp, %rsp
  pop %rbp
  pop %rsi
  pop %rdi
  pop %BYTECODE
  lea (%rsp, %rdi, 8), %rsp
  push %rax
_restore_scope:
  test $1, %rsi
  jnz _skip

  mov %VM, %rdi // VM::m_scope : Scope
  // %rsi is the Closure *
  CCALL SYMBOL(finishClosure)
_skip:
  SKIP 1

_op_lookup_slow_path:
  READ 1, %rsi // string ID
  mov STRINGS(%rip), %r9
  mov (%r9, %rsi, 8), %rsi // actual char *
  mov (%VM), %r9 // VM::m_scope *

_op_lookup_load:
  mov (%r9), %rax // Scope::table *
  test %rax, %rax
  jz _op_lookup_check_parent
  mov 0x18(%r9), %edx // Scope::tableHash
  mov %esi, %ecx // index
  and %edx, %ecx // index &= hash
  mov %ecx, %r8d // begin

_op_lookup_begin:
  mov %ecx, %edi
  shl $1, %edi
  mov (%rax, %rdi, 0x8), %r11  // Entry::key
  test %r11,%r11
  jz _op_lookup_check_parent
  cmp %rsi, %r11
  jz _op_lookup_found
  inc %ecx
  and %edx, %ecx
  cmp %ecx, %r8d
  jnz _op_lookup_begin

_op_lookup_check_parent:
  mov 0x8(%r9), %r9 // Scope::parent
  test %r9, %r9
  jnz _op_lookup_load

_op_lookup_not_found:
  mov %rsi, %rdi
  CCALL SYMBOL(symbolNotFound)

_op_lookup_found:
  mov 0x8(%rax, %rdi, 0x8), %rax  // Entry::value
  READ 2, %rdx
  test %rdx, %rdx
  jz _op_lookup_done
  mov %rax, (%LOOKUP, %rdx, 8) // cache value

_op_lookup_done:
  push %rax
  SKIP 2

.data
STRINGS: .quad 0
