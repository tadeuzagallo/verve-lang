#include "utils/macros.h"

.macro get_arg offset, reg
  .if \offset == 0
    movl 0x0(%rsi), \reg
  .elseif \offset == 1
    movl 0x8(%rsi), \reg
  .endif
.endmacro

.macro basic_math name, op
.globl SYMBOL(builtin_\name)
SYMBOL(builtin_\name):
  get_arg 0, %eax
  get_arg 1, %edi
  \op %edi, %eax
  ret
.endmacro

basic_math sub, subl
basic_math add, addl

.globl SYMBOL(builtin_lt)
SYMBOL(builtin_lt):
  get_arg 0, %eax
  get_arg 1, %edi
  cmpl %edi, %eax
  setl %al
  movzbl %al, %eax
  ret
