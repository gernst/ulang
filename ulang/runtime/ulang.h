// the runtime of compiled ulang programs, where every value is a pointer to an object on the heap,
// which the Boehm collector frees, integers are unbounded with GMP, and exceptions jump to handlers

#ifndef ULANG_H
#define ULANG_H

// stdio.h before gmp.h, which declares its functions on files only after it
#include <stdio.h>
#include <gmp.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdint.h>

typedef struct u_object *u_value;

// the type of a value, with its order, which gives -1, 0, or 1, its equality, which agrees with the order, and its text,
// where each type exists once, the built-in ones in the runtime and each datatype in the compiled program, whose
// descriptor points to the functions compare, ==, and show that its module declares, which have the same signatures
struct u_type {
  const char *name;
  u_value (*compare)(u_value a, u_value b);
  u_value (*equals)(u_value a, u_value b);
  u_value (*show)(u_value v);
};

// the name of a constructor with its module, its position in its declaration, which orders values, and its datatype,
// where each name exists once and is compared by its address
struct u_name {
  const char *module;
  const char *base;
  int index;
  const struct u_type *type;
};

// the built-in types, where integers that fit into a long long are of u_int_type with num, and others of u_big_type
// with an mpz_t, such that each integer has one representation
extern const struct u_type u_int_type, u_big_type, u_bool_type, u_char_type, u_str_type, u_tuple_type, u_closure_type, u_proc_type,
  u_array_type;

// the datatypes whose constructors the builtins create, where the compiled program defines the descriptors of those of
// the prelude and its modules, which every program has, and the runtime that of OsError, which programs need not import
extern const struct u_type t_list_46_List, t_option_46_Option, t_os_46_OsError;
extern const struct u_type t_prelude_46_DivisionByZero, t_prelude_46_OutOfBounds, t_prelude_46_NoCharacter,
  t_prelude_46_MissingField;

// the order, equality, and text of datatypes by their constructors and then their fields, which the descriptors
// of datatypes use unless their module declares compare, ==, or show, and the equality of an order
u_value u_con_compare(u_value a, u_value b);
u_value u_equals_by_compare(u_value a, u_value b);
u_value u_con_show(u_value v);

// the constructors that the builtins create, of the modules list, option, and os of the library
extern const struct u_name n_list_46_Nil, n_list_46_Cons, n_option_46_None, n_option_46_Some, n_os_46_OsError;

// the exceptions of the builtins and of accessors of fields, which the prelude declares
extern const struct u_name n_prelude_46_DivisionByZero, n_prelude_46_OutOfBounds, n_prelude_46_NoCharacter,
  n_prelude_46_MissingField;

// the code of a closure, with its captured values and its arguments
typedef u_value (*code)(u_value *env, u_value *args);

// the code of a procedure as a value, with its captured values, its ins, and pointers to its outs and refs
typedef void (*proc_code)(u_value *env, u_value *ins, u_value **outs);

// the fields of the type of an object, where constructors, tuples, closures, and arrays have items
struct u_object {
  const struct u_type *type;
  union {
    mpz_t big;
    long long num;
    // strings of code points, with their number, stored as units of 1, 2, or 4 bytes each, the narrowest that holds
    // every code point of the string, such that equal strings have equal units, where units of 1 byte end with a 0
    struct {
      const void *units;
      long long length;
      int width;
    };
    struct {
      union {
        const struct u_name *name;
        code fun;
        proc_code proc;
      };
      int size;
      u_value *items;
    };
  };
};

u_value u_mk_int(long long n);
u_value u_mk_int_str(const char *digits);
u_value u_mk_bool(int b);
u_value u_mk_char(long long c);
u_value u_mk_str(const char *s);
u_value u_mk_units(const void *units, long long n, int width);

// a string literal, "…" of code points below 256, u"…" below 65536, or U"…", of the narrowest kind that holds its code
// points, whose length the C compiler knows
#define u_mk_lit(s) u_mk_units(s, sizeof(s) / sizeof(s[0]) - 1, sizeof(s[0]))

// the code point at index i of a string
static inline long long u_str_at(u_value s, long long i) {
  if (s->width == 1) return ((const unsigned char *)s->units)[i];
  if (s->width == 2) return ((const uint16_t *)s->units)[i];
  return ((const uint32_t *)s->units)[i];
}

// the UTF-8 of a string, ending with a 0, with its number of bytes in n unless n is NULL, and the string of UTF-8, or
// else NULL if it is not valid
const char *u_utf8(u_value s, size_t *n);
u_value u_from_utf8(const char *bytes, size_t n);

// the cache of a function without arguments while its value is computed, which is no object
#define U_COMPUTING ((u_value)-1)

// the initializer of a static object of a string literal as of u_mk_lit, which needs no allocation
#define U_LIT(s) {.type = &u_str_type, .units = s, .length = sizeof(s) / sizeof(s[0]) - 1, .width = sizeof(s[0])}
u_value u_mk_con(const struct u_name *name, int size, ...);
u_value u_mk_con_array(const struct u_name *name, int size, u_value *items);
u_value u_mk_tuple(int size, ...);
u_value u_mk_closure(code fun, int size, ...);
u_value u_mk_proc(proc_code proc, int size, ...);

// the booleans and the empty list, which exist once
extern struct u_object u_true, u_false;

// the shared value of a constructor without arguments, with descriptor n of datatype t, where C needs t, since a static
// initializer cannot read it from n
#define U_UNIT(n, t) {.type = &t, .name = &n, .size = 0, .items = NULL}

// the values of the constructors without arguments that the builtins create
extern struct u_object o_list_46_Nil, o_option_46_None, o_prelude_46_DivisionByZero;

static inline int u_truth(u_value v) {
  return v->num != 0;
}

// v is a value of the datatype of name, which the checker guarantees
static inline int u_is_con(u_value v, const struct u_name *name) {
  return v->name == name;
}

static inline u_value u_field(u_value v, int i) {
  return v->items[i];
}

// the field of an accessor T.f, at the position ix[k] in the constructor k of v, by the index of k, which is the position
// of k in its declaration, or else MissingField with the name T.f of the field, and v with y as the field for the
// updater T.f:=
void u_missing_field(u_value field);
u_value u_update(u_value v, const int *ix, u_value field, u_value y);

static inline u_value u_select(u_value v, const int *ix, u_value field) {
  int i = ix[v->name->index];
  if (i < 0) u_missing_field(field);
  return v->items[i];
}

int u_equals(u_value a, u_value b);
u_value u_compare(u_value a, u_value b);
u_value u_apply(u_value f, int n, ...);
void u_call_proc(u_value p, int ins, int outs, ...);

u_value u_add(u_value a, u_value b);
u_value u_sub(u_value a, u_value b);
u_value u_mul(u_value a, u_value b);
u_value u_div(u_value a, u_value b);
u_value u_mod(u_value a, u_value b);
u_value u_lt(u_value a, u_value b);
u_value u_eq(u_value a, u_value b);
u_value u_chars(u_value s);
u_value u_string(u_value cs);
u_value u_ord(u_value c);
u_value u_chr(u_value n);
u_value *u_append(u_value *end, u_value x);
u_value u_concat(u_value s, u_value t);
// the functions of the builtin section of str.u
u_value u_size(u_value s);
u_value u_at(u_value i, u_value s);
u_value u_contains(u_value c, u_value s);
u_value u_slice(u_value s, u_value i, u_value j);
u_value u_join(u_value sep, u_value ss);
u_value u_split(u_value s, u_value c);
u_value u_quote(u_value s);
u_value u_number(u_value s);

// the functions of the builtin section of array.u, where arrays are objects of u_array_type with their elements as items
u_value u_array(u_value xs);
u_value u_mk_array(int n, ...);
int u_count(u_value c);
u_value u_array_new(int n);
u_value u_array_push(u_value a, u_value x);
u_value u_array_done(u_value a, int n);
u_value u_array_list(u_value a);
u_value u_array_size(u_value a);
u_value u_array_at(u_value i, u_value a);
u_value u_array_put(u_value i, u_value x, u_value a);
u_value u_array_slice(u_value a, u_value i, u_value j);
u_value u_array_foldr(u_value a, u_value acc, u_value f);
u_value u_array_foldl(u_value a, u_value acc, u_value f);

u_value u_is_digit(u_value c);
u_value u_is_letter(u_value c);
u_value u_is_space(u_value c);

// the text of a value as it is printed, by the descriptors of its objects, which use the shows that datatypes declare
u_value u_show(u_value v);

// the texts of the runtime errors, which every program defines from failure_formats of ulang/error.u, where %s stands for a
// value as show gives it
extern const char *const u_text_no_case, *const u_text_compare_functions, *const u_text_compare_procedures,
  *const u_text_assertion_failed, *const u_text_assertion_unequal, *const u_text_uncaught;

void u_no_case(u_value v);
void u_assert_true(u_value v);
void u_assert_equal(u_value a, u_value b);
void u_fail(const char *msg);

// the handlers of the trys in progress, innermost first, where a raise jumps to the innermost one
struct u_handler {
  jmp_buf env;
  struct u_handler *next;
  u_value value;
  const char *type;
};

void u_args(int argc, char **argv);
void p_os_args(u_value *args);
void p_os_read(u_value path, u_value *text);
void p_os_print(u_value text);
void p_os_print_error(u_value text);
void p_os_exit(u_value code);
void u_passed(const char *name);
void p_os_write(u_value path, u_value text);
void p_os_list(u_value dir, u_value *entries);
void p_os_is_dir(u_value path, u_value *yes);
void p_os_cwd(u_value *dir);
void p_os_chdir(u_value dir);
void p_os_library(u_value name, u_value *text);


struct u_handler *u_push(void);
void u_pop(struct u_handler *h);
int u_has_type(struct u_handler *h, const char *type);
u_value u_caught(struct u_handler *h);
void u_raise(u_value v, const char *type);
void u_reraise(struct u_handler *h);

#endif
