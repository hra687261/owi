f**.reinterpret_i** i**.reinterpret_f** instructions:
  $ owi opt reinterpret.wat > reinterpret.opt.wat
  $ cat reinterpret.opt.wat
  (module
    (func $start
      
    )
    (start 0)
  )
  $ owi run reinterpret.opt.wat
