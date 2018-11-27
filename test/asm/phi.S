  .text
phi_32:
  .args   0, i32

  arg.i32 $0, 0
  jt $0, .Ltrue
.Lfalse:
  jmp .Lend
.Ltrue:
  jmp .Lend
.Lend:
  phi.i32 $1, .Lfalse, 0, .Ltrue, 1
  ret.i32 $1


phi_64:
  .args 0, i64

  arg.i64 $0, 0
  jt $0, .Ltrue
.Lfalse:
  jmp .Lend
.Ltrue:
  jmp .Lend
.Lend:
  phi.i64 $1, .Lfalse, 0, .Ltrue, 1
  ret.i64 $1


phi_expand:
  .args 0, i64

  arg.i64 $0, 0
  jt $0, .Ltrue
.Lfalse:
  mov.i64 $1, 123
  jmp .Lend
.Ltrue:
  mov.i64 $1, 124
  jmp .Lend
.Lend:
  ret.i64 $1


phi_f32:
  .args 0, i64

  arg.i64 $0, 0
  jt $0, .Ltrue
.Lfalse:
  jmp .Lend
.Ltrue:
  jmp .Lend
.Lend:
  phi.f32 $1, .Lfalse, 0, .Ltrue, 0xC2F63EFA
  ret.f32 $1


