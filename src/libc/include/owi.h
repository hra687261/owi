#ifndef _OWI_H
#define _OWI_H


#ifdef __cplusplus
extern "C"
{
#endif
__attribute__((import_module("summaries"), import_name("alloc"))) void *
owi_malloc(void *, unsigned int);
__attribute__((import_module("summaries"), import_name("dealloc"))) void *
owi_free(void *);

__attribute__((import_module("symbolic"), import_name("i8_symbol"))) char
owi_i8(void);
__attribute__((import_module("symbolic"), import_name("char_symbol"))) char
owi_char(void);
__attribute__((import_module("symbolic"), import_name("i32_symbol"))) int
owi_i32(void);
__attribute__((import_module("symbolic"), import_name("i64_symbol"))) long long
owi_i64(void);
__attribute__((import_module("symbolic"), import_name("f32_symbol"))) float
owi_f32(void);
__attribute__((import_module("symbolic"), import_name("f64_symbol"))) double
owi_f64(void);
__attribute__((import_module("symbolic"), import_name("range_symbol"))) int
owi_range(int lo, int hi);
__attribute__((import_module("symbolic"), import_name("print_char"))) void
owi_print_char(int);
__attribute__((import_module("symbolic"), import_name("label"))) void
owi_label(int id, char * name);
__attribute__((import_module("symbolic"), import_name("bool_symbol")))
#ifdef __cplusplus
  bool
#else
  _Bool
#endif
owi_bool(void);

__attribute__((import_module("symbolic"), import_name("assume"))) void
owi_assume(int);
__attribute__((import_module("symbolic"), import_name("assert"))) void
owi_assert(int);

__attribute__((import_module("summaries"))) __attribute__((import_name("abort"))) void
owi_abort(void);
__attribute__((import_module("summaries"))) __attribute__((import_name("exit"))) void
owi_exit(int);

#ifdef __cplusplus
}
#endif


#endif
