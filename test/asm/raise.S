  .code
_raise:
  .call caml

  mov.i64 $7, 8
  mov.i64 $8, _exc_ptr
  # movq  _exc_ptr(%rip), %rax
  ld.8.i64 $3, [$8]
  # movq  -8(%rax), %rsp
  sub.i64 $4, $3, $7
  ld.8.i64 $6, [$4]
  set.i64 $rsp, $6
  # movq  -16(%rax), %rcx
  sub.i64 $5, $4, $7
  ld.8.i64 $1, [$5]
  # movq  %rcx, _exc_ptr(%rip)
  st.8 [$8], $1
  # jmpq  *(%rax)
  ld.8.i64 $2, [$3]
  ji $2


  .data data
_exc_ptr:
  .quad 0

  .code
_try_catch:
  .call caml
  .stack 24
  .args 0

  # subq  $24, %rsp
  # leaq  .L104(%rip), %rax
  mov.i64 $2, .L104
  frame.i64 $3, 0
  # movq  %rax, (%rsp)
  st.8 [$3], $2
  jmp .L102
.L102:
  mov.i64 $15, _exc_ptr
  frame.i64 $16, 0
  frame.i64 $17, 8
  frame.i64 $18, 16
  # movq  _exc_ptr(%rip), %rax
  ld.8.u64 $19, [$15]
  # movq  %rax, 16(%rsp)
  st.8 [$18], $19
  # movq  %rsp, %rax
  mov.i64 $20, $rsp
  # movq  %rsp, 8(%rsp)
  st.8 [$17], $20
  # movq  %rax, _exc_ptr(%rip)
  st.8 [$15], $16

  # callq _dummy
  mov.i64 $22, _dummy
  invoke.i64.c $1002, $22, .L104  @caml_frame

  mov.i64 $23, _exc_ptr
  frame.i64 $25, 16
  # movq  16(%rsp), %rcx
  ld.8.i64 $24, [$25]
  # movq  %rcx, _exc_ptr(%rip)
  st.8 [$23], $24
  # addq  $24, %rsp
  # retq
  ret.i64 $1002

.L104:
  # xorl %eax, %eax
  # addq $24, %rsp
  # retq
  mov.i64 $0, 0
  ret.i64 $0

_dummy:
  # xorl %eax, %eax
  # retq
  mov.i64 $0, 0
  ret.i64 $0
