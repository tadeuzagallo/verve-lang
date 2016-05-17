.macro get_arg
  .if $0 == 0
    mov 0x0(%rsi), $1
  .elseif $0 == 1
    mov 0x8(%rsi), $1
  .endif
.endmacro

.macro basic_math
.globl _builtin_$0
_builtin_$0:
  get_arg 0, %eax
  get_arg 1, %edi
  $1 %edi, %eax
  $2
  ret
.endmacro

basic_math sub, sub
basic_math add, add

.globl _builtin_lt
_builtin_lt:
  get_arg 0, %eax
  get_arg 1, %edi
  cmp %edi, %eax
  setl %al
  movzbl %al, %eax
  ret
