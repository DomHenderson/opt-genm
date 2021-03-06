caml_curry2_1:
  .visibility hidden
  .call                caml
  .args                0, i64, i64
  arg.i64              $10001, 0 @caml_value
  arg.i64              $10009, 1 @caml_value
  mov.i64              $0, $10001 @caml_value
  mov.i64              $1, $10009 @caml_value
  mov.i64              $15000, 24
  add.i64              $15001, $1, $15000
  ld.8.u64             $2, [$15001] @caml_value
  mov.i64              $15002, 16
  add.i64              $15003, $1, $15002
  ld.8.u64             $1, [$15003] @caml_value
  mov.i64              $15004, 16
  add.i64              $15005, $2, $15004
  ld.8.u64             $3, [$15005] @caml_value
  mov.i64              $10000, $1 @caml_value
  mov.i64              $10008, $0 @caml_value
  mov.i64              $10016, $2 @caml_value
  tcall.i64.caml       $3, $10000, $10008, $10016 @caml_frame

  caml_curry17_16:
  .visibility hidden
  .call                caml
  .args                0, i64, i64
  arg.i64              $10001, 0 @caml_value
  arg.i64              $10009, 1 @caml_value
  mov.i64              $0, $10001 @caml_value
  mov.i64              $1, $10009 @caml_value
  mov.i64              $15000, 24
  add.i64              $15001, $1, $15000
  ld.8.u64             $2, [$15001] @caml_value
  mov.i64              $15002, 24
  add.i64              $15003, $2, $15002
  ld.8.u64             $3, [$15003] @caml_value
  mov.i64              $15004, 24
  add.i64              $15005, $3, $15004
  ld.8.u64             $4, [$15005] @caml_value
  mov.i64              $15006, 24
  add.i64              $15007, $4, $15006
  ld.8.u64             $5, [$15007] @caml_value
  mov.i64              $15008, 24
  add.i64              $15009, $5, $15008
  ld.8.u64             $6, [$15009] @caml_value
  mov.i64              $15010, 24
  add.i64              $15011, $6, $15010
  ld.8.u64             $7, [$15011] @caml_value
  mov.i64              $15012, 24
  add.i64              $15013, $7, $15012
  ld.8.u64             $8, [$15013] @caml_value
  mov.i64              $15014, 24
  add.i64              $15015, $8, $15014
  ld.8.u64             $9, [$15015] @caml_value
  mov.i64              $15016, 24
  add.i64              $15017, $9, $15016
  ld.8.u64             $10, [$15017] @caml_value
  mov.i64              $15018, 24
  add.i64              $15019, $10, $15018
  ld.8.u64             $11, [$15019] @caml_value
  mov.i64              $15020, 24
  add.i64              $15021, $11, $15020
  ld.8.u64             $12, [$15021] @caml_value
  mov.i64              $15022, 24
  add.i64              $15023, $12, $15022
  ld.8.u64             $13, [$15023] @caml_value
  mov.i64              $15024, 24
  add.i64              $15025, $13, $15024
  ld.8.u64             $14, [$15025] @caml_value
  mov.i64              $15026, 24
  add.i64              $15027, $14, $15026
  ld.8.u64             $15, [$15027] @caml_value
  mov.i64              $15028, 24
  add.i64              $15029, $15, $15028
  ld.8.u64             $16, [$15029] @caml_value
  mov.i64              $15030, 24
  add.i64              $15031, $16, $15030
  ld.8.u64             $17, [$15031] @caml_value
  mov.i64              $15032, 16
  add.i64              $15033, $1, $15032
  ld.8.u64             $1, [$15033] @caml_value
  mov.i64              $15034, 16
  add.i64              $15035, $2, $15034
  ld.8.u64             $2, [$15035] @caml_value
  mov.i64              $15036, 16
  add.i64              $15037, $3, $15036
  ld.8.u64             $3, [$15037] @caml_value
  mov.i64              $15038, 16
  add.i64              $15039, $4, $15038
  ld.8.u64             $4, [$15039] @caml_value
  mov.i64              $15040, 16
  add.i64              $15041, $5, $15040
  ld.8.u64             $5, [$15041] @caml_value
  mov.i64              $15042, 16
  add.i64              $15043, $6, $15042
  ld.8.u64             $6, [$15043] @caml_value
  mov.i64              $15044, 16
  add.i64              $15045, $7, $15044
  ld.8.u64             $7, [$15045] @caml_value
  mov.i64              $15046, 16
  add.i64              $15047, $8, $15046
  ld.8.u64             $8, [$15047] @caml_value
  mov.i64              $15048, 16
  add.i64              $15049, $9, $15048
  ld.8.u64             $9, [$15049] @caml_value
  mov.i64              $15050, 16
  add.i64              $15051, $10, $15050
  ld.8.u64             $10, [$15051] @caml_value
  mov.i64              $15052, 16
  add.i64              $15053, $11, $15052
  ld.8.u64             $11, [$15053] @caml_value
  mov.i64              $15054, 16
  add.i64              $15055, $12, $15054
  ld.8.u64             $12, [$15055] @caml_value
  mov.i64              $15056, 16
  add.i64              $15057, $13, $15056
  ld.8.u64             $13, [$15057] @caml_value
  mov.i64              $15058, 16
  add.i64              $15059, $14, $15058
  ld.8.u64             $14, [$15059] @caml_value
  mov.i64              $15060, 16
  add.i64              $15061, $15, $15060
  ld.8.u64             $15, [$15061] @caml_value
  mov.i64              $15062, 16
  add.i64              $15063, $16, $15062
  ld.8.u64             $16, [$15063] @caml_value
  mov.i64              $15064, 16
  add.i64              $15065, $17, $15064
  ld.8.u64             $18, [$15065] @caml_value
  mov.i64              $10000, $16 @caml_value
  mov.i64              $10008, $15 @caml_value
  mov.i64              $10016, $14 @caml_value
  mov.i64              $10024, $13 @caml_value
  mov.i64              $10032, $12 @caml_value
  mov.i64              $10040, $11 @caml_value
  mov.i64              $10048, $10 @caml_value
  mov.i64              $10056, $9 @caml_value
  mov.i64              $10064, $8 @caml_value
  mov.i64              $10072, $7 @caml_value
  mov.i64              $10080, $6 @caml_value
  mov.i64              $10088, $5 @caml_value
  mov.i64              $10096, $4 @caml_value
  mov.i64              $10104, $3 @caml_value
  mov.i64              $10112, $2 @caml_value
  mov.i64              $10120, $1 @caml_value
  mov.i64              $10128, $0 @caml_value
  mov.i64              $10136, $17 @caml_value
  tcall.i64.caml       $18, $10000, $10008, $10016, $10024, $10032, $10040, $10048, $10056, $10064, $10072, $10080, $10088, $10096, $10104, $10112, $10120, $10128, $10136 @caml_frame
