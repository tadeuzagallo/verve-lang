#define STRING_TAG $0x2
#define CLOSURE_TAG $0x0800000000000000
#define BYTECODE %r12
#define STRINGS %r13
#define VM %r14

.macro read
  mov $0, %rax
  mov -0x3(BYTECODE, %rax, 4), $1
.endmacro

.macro skip
  mov $0, %rax
  lea 0x1(BYTECODE, %rax, 4), BYTECODE
  jmp _loop
.endmacro

.macro unmask
  shl $$0x16, $0
  shr $$0x16, $0
.endmacro

.macro ccall
  push %r15
  mov %rsp, %r15
  and $-0x10, %rsp
  call $0
  mov %r15, %rsp
  pop %r15
.endmacro

.globl _execute
_execute:
  push %rbp
  push BYTECODE
  push STRINGS
  push VM
  mov %rsp, %rbp
  mov %rdi, BYTECODE
  mov %rsi, STRINGS
  mov %rdx, VM

_loop:
  xor %rax, %rax
  mov (BYTECODE), %al // 0..10
  lea (%rip), %rdi
  lea 0x7(%rdi, %al, 8), %rdi
  jmp *%rdi
  jmp _op_ret
  nop
  nop
  nop
  jmp _op_bind
  nop
  nop
  nop
  jmp _op_push
  nop
  nop
  nop
  jmp _op_call
  nop
  nop
  nop
  jmp _op_jz
  nop
  nop
  nop
  jmp _op_jmp
  nop
  nop
  nop
  jmp _op_create_closure
  nop
  nop
  nop
  jmp _op_load_string
  nop
  nop
  nop
  jmp _op_push_arg
  nop
  nop
  nop
  jmp _op_lookup
  nop
  nop
  nop
  jmp _op_exit

_op_exit:
  mov %rbp, %rsp
  pop VM
  pop STRINGS
  pop BYTECODE
  pop %rbp
  ret

_op_push:
  read $1, %edi
  push %rdi
  skip $1

_op_lookup:
  mov VM, %rdi
  read $1, %esi
  mov (STRINGS, %rsi, 8), %rsi
  ccall _getScope
  push %rax
  skip $1

_op_call:
  // pop the callee from the stack
  pop %r8

  // setup args
  read $1, %edi //argc
  mov %rsp, %rsi // argv just live in the stack
  mov VM, %rdx

  // setup callee
  unmask %r8
  ccall *%r8

  push %rax
  skip $1

_op_load_string:
  read $1, %edi
  mov (STRINGS, %rdi, 8), %rdi
  rol $8, %rdi 
  mov STRING_TAG, %dil
  ror $8, %rdi
  push %rdi
  skip $1

_op_create_closure:
  hlt

_op_bind:
  hlt

_op_jz:
  read $1, %edi
  pop %rsi
  test %esi, %esi
  jz _jz
  skip $1
_jz:
  add %rdi, BYTECODE
  jmp _loop


_op_jmp:
  read $1, %edi
  add %rdi, BYTECODE
  jmp _loop

_op_ret:
  hlt

_op_push_arg:
  hlt
