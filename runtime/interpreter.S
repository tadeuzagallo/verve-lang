#define STRING_TAG    1 << 1
#define LIST_TAG     1 << 2
#define CLOSURE_TAG   1 << 4

#define BYTECODE r12
#define SCOPE_STACK r13
#define VM r14
#define BCBASE r15
#define LOOKUP rbx

.macro read
.if $0 == 1
  mov 0x8(%BYTECODE), $1
.elseif $0 == 2
  mov 0x10(%BYTECODE), $1
.else
  hlt
.endif
.endmacro

.macro skip
.if $0 == 0
  add $$0x8, %BYTECODE
.elseif $0 == 1
  add $$0x10, %BYTECODE
.elseif $0 == 2
  add $$0x18, %BYTECODE
.else
  hlt
.endif
  jmp *(%BYTECODE)
.endmacro

.macro unmask
  shl $$8, $0
  shr $$8, $0
.endmacro

.macro ccall
  push %rbx
  mov %rsp, %rbx
  and $-0x10, %rsp
  call $0
  mov %rbx, %rsp
  pop %rbx
.endmacro

.macro get_arg
  mov 0x20(%rbp, $0, 8), $1
.endmacro

.globl _execute
_execute:
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

.globl _op_exit
_op_exit:
  mov %rbp, %rsp
  pop %LOOKUP
  pop %BCBASE
  pop %VM
  pop %BYTECODE
  pop %rbp
  ret

.globl _op_lookup
_op_lookup:
_op_lookup_fast_path:
  read 2, %rdi
  mov (%LOOKUP, %rdi, 8), %rsi // Cached address
  test %rsi, %rsi
  jz _op_lookup_slow_path
  push %rsi
  skip 2

.globl _op_push
_op_push:
  read 1, %rdi
  push %rdi
  skip 1

.globl _op_push_arg
_op_push_arg:
  read 1, %rdi
  get_arg %rdi, %rax
  push %rax
  skip 1

.globl _op_jz
_op_jz:
  pop %rdi
  test %rdi, %rdi
  jz _jz
  skip 1
_jz:
  read 1, %rdi
  add %rdi, %BYTECODE
  jmp *(%BYTECODE)

.globl _op_jmp
_op_jmp:
  read 1, %rdi
  add %rdi, %BYTECODE
  jmp *(%BYTECODE)

.globl _op_call
_op_call:
  // pop the callee from the stack
  pop %rcx

  // setup args
  read 1, %rdi //argc
  mov %rsp, %rsi // argv just lives in the stack
  mov %VM, %rdx

  // check tag
  rol $8, %rcx
  test $CLOSURE_TAG, %cl
  jnz _op_call_closure

_op_call_builtin:
  shr $8, %rcx
  push %rdi
  ccall *%rcx
  pop %rdi
  lea (%rsp, %rdi, 8), %rsp
  push %rax
  skip 1

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
  ccall _prepareClosure
  lea (%BCBASE, %rax, 1), %BYTECODE
  jmp *(%BYTECODE)

_op_call_fast_closure:
  shr $1, %ecx
  lea (%BCBASE, %rcx, 1), %BYTECODE
  jmp *(%BYTECODE)


.globl _op_load_string
_op_load_string:
  read 1, %rdi
  mov STRINGS(%rip), %rsi
  mov (%rsi, %rdi, 8), %rdi
  rol $8, %rdi
  mov $STRING_TAG, %dil
  ror $8, %rdi
  push %rdi
  skip 1

.globl _op_create_closure
_op_create_closure:
  mov %VM, %rdi
  read 1, %rsi
  read 2, %rdx
  ccall _createClosure
  push %rax
  skip 2

.globl _op_bind
_op_bind:
  mov %VM, %rdi
  read 1, %rsi
  pop %rdx
  mov STRINGS(%rip), %rcx
  mov (%rcx, %rsi, 8), %rsi
  ccall _setScope
  skip 1

.globl _op_create_lex_scope
_op_create_lex_scope:
  mov %VM, %rdi
  ccall _pushScope
  skip 0

.globl _op_release_lex_scope
_op_release_lex_scope:
  mov %VM, %rdi
  ccall _restoreScope
  skip 0

.globl _op_put_to_scope
_op_put_to_scope:
  mov %VM, %rdi
  read 1, %rsi
  read 2, %rdx

  mov STRINGS(%rip), %rcx
  mov (%rcx, %rsi, 8), %rsi
  get_arg %rdx, %rdx

  ccall _setScope
  skip 2

.globl _op_alloc_obj
_op_alloc_obj:
  mov %VM, %rdi
  read 1, %rsi
  call _allocate
  push %rax
  skip 1

.globl _op_alloc_list
_op_alloc_list:
  mov %VM, %rdi
  read 1, %rsi
  call _allocate
  read 1, %rsi
  dec %rsi
  mov %rsi, (%rax)
  rol $8, %rax
  mov $LIST_TAG, %al
  ror $8, %rax
  push %rax
  skip 1

.globl _op_obj_store_at
_op_obj_store_at:
  pop %rdi // value
  pop %rdx // object
  mov %rdx, %rcx
  unmask %rdx
  read 1, %rsi // index
  mov %rdi, (%rdx, %rsi, 8)
  push %rcx
  skip 1

.globl _op_obj_tag_test
_op_obj_tag_test:
  pop %rdi // object
  unmask %rdi
  read 1, %rsi // expected tag
  mov (%rdi), %rdi // object's tag
  test %rdi, %rsi
  je _op_obj_tag_test_ok
  hlt
_op_obj_tag_test_ok:
  skip 1

.globl _op_obj_load
_op_obj_load:
  pop %rdi // object
  unmask %rdi
  read 1, %rsi // offset
  mov 0x8(%rdi, %rsi, 8), %rdi // skip tag
  push %rdi
  skip 1

.globl _op_stack_alloc
_op_stack_alloc:
  push %SCOPE_STACK
  read 1, %rdi
  sub %rdi, %rsp
  mov %rsp, %SCOPE_STACK
  skip 1

.globl _op_stack_store
_op_stack_store:
  read 1, %rdi // slot - offset on stack
  pop %rsi
  mov %rsi, (%SCOPE_STACK, %rdi, 8)
  skip 1

.globl _op_stack_load
_op_stack_load:
  read 1, %rdi // slot - offset on stack
  mov (%SCOPE_STACK, %rdi, 8), %rdi
  push %rdi
  skip 1

.globl _op_stack_free
_op_stack_free:
  pop %rdx
  mov %SCOPE_STACK, %rsp
  read 1, %rdi
  add %rdi, %rsp
  pop %SCOPE_STACK
  push %rdx
  skip 1

.globl _op_ret
_op_ret:
  pop %rax
  mov %rbp, %rsp
  pop %rbp
  pop %rsi
  pop %rdi
  pop %BYTECODE
  lea (%rsp, %rdi, 8), %rsp
  push %rax
restore_scope:
  test $1, %rsi
  jnz skip

  mov %VM, %rdi // VM::m_scope : Scope
  // %rsi is the Closure *
  ccall _finishClosure
skip:
  skip 1

_op_lookup_slow_path:
  read 1, %rsi // string ID
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
  ccall _symbolNotFound

_op_lookup_found:
  mov 0x8(%rax, %rdi, 0x8), %rax  // Entry::value
  read 2, %rdx
  test %rdx, %rdx
  jz _op_lookup_done
  mov %rax, (%LOOKUP, %rdx, 8) // cache value

_op_lookup_done:
  push %rax
  skip 2

.data
STRINGS: .quad 0
