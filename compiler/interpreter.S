#define STRING_TAG 0x2
#define CLOSURE_TAG 0x10
#define BYTECODE r12
#define STRINGS r13
#define VM r14
#define BCBASE r15

.macro read
.if $0 == 1
  mov 0x1(%BYTECODE), $1
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

.globl _execute
_execute:
  push %rbp
  push %BYTECODE
  push %STRINGS
  push %VM
  push %BCBASE
  mov %rsp, %rbp
  mov %rdi, %BYTECODE
  mov %rsi, %STRINGS
  mov %rdx, %VM
  mov %rcx, %BCBASE

_loop:
  lea loop_opcodes(%rip), %rdi
  xor %rax, %rax
  mov (%BYTECODE), %al // 0..10
  lea (%rdi, %al, 8), %rdi
  jmp *%rdi
loop_opcodes:
  jmp _op_ret
  .fill 3
  jmp _op_bind
  .fill 3
  jmp _op_push
  .fill 3
  jmp _op_call
  .fill 3
  jmp _op_jz
  .fill 3
  jmp _op_jmp
  .fill 3
  jmp _op_create_closure
  .fill 3
  jmp _op_load_string
  .fill 3
  jmp _op_push_arg
  .fill 3
  jmp _op_lookup
  .fill 3
  jmp _op_exit
  .fill 3
  jmp _op_create_lex_scope
  .fill 3
  jmp _op_release_lex_scope
  .fill 3
  jmp _op_put_to_scope

_op_exit:
  mov %rbp, %rsp
  pop %BCBASE
  pop %VM
  pop %STRINGS
  pop %BYTECODE
  pop %rbp
  ret

_op_push:
  read 1, %edi
  push %rdi
  skip 1

_op_lookup:
  read 1, %esi // string ID
  mov (%STRINGS, %rsi, 8), %rsi // actual char *
  mov (%VM), %r9 // VM::m_scope *

load:
  mov (%r9), %rax // Scope::table *
  test %rax, %rax
  jz check_parent
  mov 0x18(%r9), %edx // Scope::tableHash
  mov %esi, %ecx // index
  and %edx, %ecx // index &= hash
  mov %ecx, %r8d // begin

begin:
  mov %ecx, %edi
  shl $1, %edi
  mov (%rax, %rdi, 0x8), %r11  // Entry::key
  test %r11,%r11
  jz check_parent
  cmp %rsi, %r11
  jz found
  inc %ecx
  and %edx, %ecx
  cmp %ecx, %r8d
  jnz begin

check_parent:
  mov 0x8(%r9), %r9 // Scope::parent
  test %r9, %r9
  jz not_found
  jmp load

not_found:
  mov %rsi, %rdi
  ccall _symbolNotFound

found:
  mov 0x8(%rax, %rdi, 0x8), %rax  // Entry::value

done:
  push %rax
  skip 1

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
  jnz call_closure

call_builtin:
  shr $8, %rcx
  push %rdi
  ccall *%rcx
  pop %rdi
  lea (%rsp, %rdi, 8), %rsp
  push %rax
  skip 1

call_closure:
  shr $8, %rcx
  push %BYTECODE
  push %rdi
  push %rcx
  push %rbp
  mov %rsp, %rbp
  ccall _prepareClosure
  lea (%BCBASE, %rax, 1), %BYTECODE
  jmp _loop

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
  mov %VM, %rdi // VM::m_scope : Scope
  // %rsi is the Closure *
  ccall _finishClosure
  skip 1

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
  pop %rdx
  mov %rdx, %rsi
  unmask %rsi
  mov 0x8(%rsi), %rsi
  mov (%rsi), %esi
  mov (%STRINGS, %rsi, 8), %rsi
  ccall _setScope
  skip 0

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

_op_push_arg:
  read 1, %edi
  get_arg %rdi, %rax
  push %rax
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
