#define STRING_TAG    1 << 1
#define CLOSURE_TAG   1 << 4

#define BYTECODE r12
#define STRINGS r13
#define VM r14
#define BCBASE r15
#define LOOKUP rbx

.macro read
.if $0 == 1
  mov 0x1(%BYTECODE), $1
.elseif $0 == 2
  mov 0x5(%BYTECODE), $1
.else
  mov $$$0, %rax
  mov -0x3(%BYTECODE, %rax, 4), $1
.endif
.endmacro

.macro skip
.if $0 == 0
  inc %BYTECODE
.elseif $0 == 1
  add $$0x5, %BYTECODE
.elseif $0 == 2
  add $$0x9, %BYTECODE
.else
  mov $$$0, %rax
  lea 0x1(%BYTECODE, %rax, 4), %BYTECODE
.endif
  jmp _loop
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

.macro op_address
  .long _op_$0 - _loop_op_addresses
.endmacro

.globl _execute
_execute:
  push %rbp
  push %BYTECODE
  push %STRINGS
  push %VM
  push %BCBASE
  push %LOOKUP
  mov %rsp, %rbp
  mov %rdi, %BYTECODE
  mov %rsi, %STRINGS
  mov %rdx, %VM
  mov %rcx, %BCBASE
  mov %r8,  %LOOKUP
  jmp _loop

_op_exit:
  mov %rbp, %rsp
  pop %LOOKUP
  pop %BCBASE
  pop %VM
  pop %STRINGS
  pop %BYTECODE
  pop %rbp
  ret

_loop:
  xor %rax, %rax
  mov (%BYTECODE), %al // 0..10
  lea _loop_op_addresses(%rip), %rdi
  movsl (%rdi, %al, 4), %rax
  add %rax, %rdi
  jmp *%rdi
_loop_op_addresses:
  op_address ret
  op_address bind
  op_address push
  op_address call
  op_address jz
  op_address jmp
  op_address create_closure
  op_address load_string
  op_address push_arg
  op_address lookup
  op_address exit
  op_address create_lex_scope
  op_address release_lex_scope
  op_address put_to_scope

_op_lookup:
_op_lookup_fast_path:
  read 2, %edi
  mov (%LOOKUP, %rdi, 8), %rsi // Cached address
  test %rsi, %rsi
  jz _op_lookup_slow_path
  push %rsi
  skip 2

_op_push:
  read 1, %edi
  push %rdi
  skip 1

_op_push_arg:
  read 1, %edi
  get_arg %rdi, %rax
  push %rax
  skip 1

_op_jz:
  pop %rdi
  test %edi, %edi
  jz _jz
  skip 1
_jz:
  read 1, %edi
  add %rdi, %BYTECODE
  jmp _loop

_op_jmp:
  read 1, %edi
  add %rdi, %BYTECODE
  jmp _loop

_op_call:
  // pop the callee from the stack
  pop %rcx

  // setup args
  read 1, %edi //argc
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
  jmp _loop

_op_call_fast_closure:
  shr $1, %ecx
  lea (%BCBASE, %rcx, 1), %BYTECODE
  jmp _loop


_op_load_string:
  read 1, %edi
  mov (%STRINGS, %rdi, 8), %rdi
  rol $8, %rdi
  mov $STRING_TAG, %dil
  ror $8, %rdi
  push %rdi
  skip 1

_op_create_closure:
  mov %VM, %rdi
  read 1, %esi
  read 2, %edx
  ccall _createClosure
  push %rax
  skip 2

_op_bind:
  mov %VM, %rdi
  read 1, %esi
  pop %rdx
  mov (%STRINGS, %rsi, 8), %rsi
  ccall _setScope
  skip 1

_op_create_lex_scope:
  mov %VM, %rdi
  ccall _pushScope
  skip 0

_op_release_lex_scope:
  mov %VM, %rdi
  ccall _restoreScope
  skip 0

_op_put_to_scope:
  mov %VM, %rdi
  read 1, %esi
  read 2, %edx

  mov (%STRINGS, %rsi, 8), %rsi
  get_arg %rdx, %rdx

  ccall _setScope
  skip 2

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
  and $1, %rsi
  jnz skip

  mov %VM, %rdi // VM::m_scope : Scope
  // %rsi is the Closure *
  ccall _finishClosure
skip:
  skip 1

_op_lookup_slow_path:
  read 1, %esi // string ID
  mov (%VM), %r9 // VM::m_scope *
  mov (%STRINGS, %rsi, 8), %rsi // actual char *

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
  read 2, %edx
  test %edx, %edx
  jz _op_lookup_done
  mov %rax, (%LOOKUP, %rdx, 8) // cache value

_op_lookup_done:
  push %rax
  skip 2
