


  .code
main:
  .visibility extern
  mov.i64   $0, ptr0 # ptr0
  ld.8.i64  $1, [$0] # ptr1
  ld.8.i64  $2, [$1] # ptr2
  ld.8.i64  $3, [$2] # f1
  call.c    $3
  ret


f1:
  .visibility hidden
  ret


  .data data
ptr0:
  .quad ptr1
  .end

ptr1:
  .quad ptr2
  .end

ptr2:
  .quad f1
  .end

pind:
  .quad f1
  .quad f2
  .quad f3
