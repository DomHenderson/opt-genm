  .code
umulo:
  .args   0, i64, i64

  arg.i64   $0, 0
  arg.i64   $1, 1
  umulo.i32 $2, $0, $1
  ret.i32   $2

uaddo:
  .args   0, i64, i64

  arg.i64   $0, 0
  arg.i64   $1, 1
  uaddo.i32 $2, $0, $1
  ret.i32   $2
