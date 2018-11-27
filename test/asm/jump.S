  .text
_jmp:
  .args        0
  .call        c
  jmp          .BB
.BB:
  ret

  .text
_jt:
  .args        0, i32
  .call        c
  arg.i32      $0, 0
  jt           $0, .Btrue
  mov.i64      $1, 0
  ret.i64      $1
.Btrue:
  mov.i64      $1, 1
  ret.i64      $1


  .text
_jf:
  .args        0, i32
  .call        c
  arg.i32      $0, 0
  jf           $0, .Btrue
  mov.i64      $1, 0
  ret.i64      $1
.Btrue:
  mov.i64      $1, 1
  ret.i64      $1

  .text
_ji:
  .args        0, i64
  .call        c
  arg.i64      $0, 0
  ji           $0