#define STRING_TAG $0x2
#define CLOSURE_TAG $0x0800000000000000
#define BYTECODE %r12
#define STRINGS %r13
#define VM %r14

.macro read
  mov $0, %rax
  mov (BYTECODE, %rax, 8), $1
.endmacro

.macro skip
  mov $0, %rax
  lea (BYTECODE, %rax, 8), BYTECODE
  jmp *(BYTECODE)
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
  jmp  *(BYTECODE)

.globl _op_exit
_op_exit:
  mov %rbp, %rsp
  pop VM
  pop STRINGS
  pop BYTECODE
  pop %rbp
  ret

.globl _op_push
_op_push:
  read $1, %rdi
  push %rdi
  skip $2

.globl _op_lookup
_op_lookup:
  mov VM, %rdi
  read $1, %rsi
  mov (STRINGS, %rsi, 8), %rsi
  ccall _getScope
  push %rax
  skip $2

.globl _op_call
_op_call:
  // setup args
  read $1, %rdi //argc
  lea (%rsp, %rdi, 8), %rsi // argv
  mov VM, %rdx

  // setup callee
  pop %r8
  unmask %r8
  ccall *%r8

  push %rax
  skip $2

.globl _op_load_string
_op_load_string:
  read $1, %rdi
  mov (STRINGS, %rdi, 8), %rdi
  rol $8, %rdi 
  mov STRING_TAG, %dil
  ror $8, %rdi
  push %rdi
  skip $2
