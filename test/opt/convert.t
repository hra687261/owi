f**.convert_i** instructions:
  $ owi opt convert.wat > convert.opt.wat
  $ cat convert.opt.wat
  (module
    (func $start
      
    )
    (start 0)
  )
  $ owi run convert.opt.wat
