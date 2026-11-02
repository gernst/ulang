// the runtime of compiled ulang programs, where every value is a pointer to an object on the heap,
// which the Boehm collector frees, integers are unbounded with GMP, and exceptions jump to handlers

#ifndef ULANG_H
#define ULANG_H

#include <gmp.h>
#include <setjmp.h>
#include <stddef.h>

typedef struct u_object *u_value;

// the name of a constructor with its module and its position in its declaration, which orders values,
// where each name exists once and is compared by its address
struct u_name {
  const char *module;
  const char *base;
  int index;
};

// the constructors that the builtins create, of the modules list, option, and os of the library
extern const struct u_name n_list_46_Nil, n_list_46_Cons, n_option_46_None, n_option_46_Some, n_os_46_OsError;

// the exceptions of the builtins and of accessors of fields, which the prelude declares
extern const struct u_name n_prelude_46_DivisionByZero, n_prelude_46_NoIndex, n_prelude_46_NoCharacter, n_prelude_46_MissingField;

// the code of a closure, with its captured values and its arguments
typedef u_value (*code)(u_value *env, u_value *args);

// the code of a procedure as a value, with its captured values, its ins, and pointers to its outs and refs
typedef void (*proc_code)(u_value *env, u_value *ins, u_value **outs);

// integers that fit into a long long are INT with num, and others are BIG with an mpz_t, such that each value has one kind
enum kind { INT, BIG, BOOL, CHAR, STR, CON, TUPLE, CLOSURE, PROC };

// the fields of the kind of an object, where constructors, tuples, and closures have items
struct u_object {
  enum kind kind;
  union {
    mpz_t big;
    long long num;
    // strings with their length, which is measured once
    struct {
      const char *text;
      long long length;
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
u_value u_mk_str_n(const char *s, long long n);

// a string literal, whose length the C compiler knows
#define u_mk_lit(s) u_mk_str_n(s, sizeof(s) - 1)

// the cache of a function without arguments while its value is computed, which is no object
#define U_COMPUTING ((u_value)-1)

// the initializer of a static object of a string literal, which needs no allocation
#define U_LIT(s) {.kind = STR, .text = s, .length = sizeof(s) - 1}
u_value u_mk_con(const struct u_name *name, int size, ...);
u_value u_mk_con_array(const struct u_name *name, int size, u_value *items);
u_value u_mk_tuple(int size, ...);
u_value u_mk_closure(code fun, int size, ...);
u_value u_mk_proc(proc_code proc, int size, ...);

// the booleans and the empty list, which exist once
extern struct u_object u_true, u_false;

// the shared value of a constructor without arguments, with descriptor n
#define U_UNIT(n) {.kind = CON, .name = &n, .size = 0, .items = NULL}

// the values of the constructors without arguments that the builtins create
extern struct u_object o_list_46_Nil, o_option_46_None, o_prelude_46_DivisionByZero;

static inline int u_truth(u_value v) {
  return v->num != 0;
}

static inline int u_is_con(u_value v, const struct u_name *name) {
  return v->kind == CON && v->name == name;
}

static inline u_value u_field(u_value v, int i) {
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
u_value u_str_size(u_value s);
u_value u_str_join(u_value sep, u_value ss);
u_value u_str_slice(u_value s, u_value i, u_value j);
u_value u_str_split(u_value s, u_value c);
u_value u_str_get(u_value s, u_value i);
u_value u_str_contains(u_value s, u_value c);
u_value u_str_compare(u_value s, u_value t);
u_value u_str_show(u_value n);
u_value u_str_quote(u_value s);
u_value u_str_number(u_value s);
u_value u_is_digit(u_value c);
u_value u_is_letter(u_value c);
u_value u_is_space(u_value c);

void u_print_value(u_value v);
void u_print_line(u_value v);
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
void p_os_show(u_value v);
void p_os_print(u_value text);
void p_os_print_error(u_value text);
void p_os_exit(u_value code);
void u_passed(const char *name);
void p_os_write(u_value path, u_value text);
void p_os_library(u_value name, u_value *text);

// read and library of the interpreter, which give the syntax of a program, are not available in compiled programs
void p_read(u_value path, u_value *program);
void p_library(u_value name, u_value *program);


struct u_handler *u_push(void);
void u_pop(struct u_handler *h);
int u_has_type(struct u_handler *h, const char *type);
u_value u_caught(struct u_handler *h);
void u_raise(u_value v, const char *type);
void u_reraise(struct u_handler *h);

#endif
