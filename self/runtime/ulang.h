// the runtime of compiled ulang programs, where every value is a pointer to an object on the heap,
// which the Boehm collector frees, integers are unbounded with GMP, and exceptions jump to handlers

#ifndef ULANG_H
#define ULANG_H

#include <gmp.h>
#include <setjmp.h>
#include <stddef.h>

typedef struct object *value;

// the code of a closure, with its captured values and its arguments
typedef value (*code)(value *env, value *args);

enum kind { INT, BOOL, CHAR, STR, CON, TUPLE, CLOSURE };

// integers are big, booleans and characters num
struct object {
  enum kind kind;
  mpz_t big;
  long long num;
  const char *text;
  int size;
  value *items;
  code fun;
};

value mk_int(long long n);
value mk_int_str(const char *digits);
value mk_bool(int b);
value mk_char(long long c);
value mk_str(const char *s);
value mk_con(const char *name, int size, ...);
value mk_con_array(const char *name, int size, value *items);
value mk_tuple(int size, ...);
value mk_closure(code fun, int size, ...);

int truth(value v);
int is_con(value v, const char *name);
value field(value v, int i);
int equal(value a, value b);
value apply(value f, int n, ...);

value u_add(value a, value b);
value u_sub(value a, value b);
value u_mul(value a, value b);
value u_div(value a, value b);
value u_mod(value a, value b);
value u_lt(value a, value b);
value u_eq(value a, value b);
value u_chars(value s);
value u_string(value cs);
value u_ord(value c);
value u_chr(value n);
value u_reverse(value xs);
value u_concat(value s, value t);
value u_str_size(value s);
value u_str_join(value sep, value ss);
value u_str_slice(value s, value i, value j);
value u_str_split(value s, value c);
value u_str_show(value n);
value u_str_number(value s);
value u_is_digit(value c);
value u_is_letter(value c);
value u_is_space(value c);

void print_value(value v);
void print_line(value v);
void print_binding(const char *name, value v);
void no_case(value v);
void assert_true(value v);
void assert_equal(value a, value b);
void fail(const char *msg);

// the handlers of the trys in progress, innermost first, where a raise jumps to the innermost one
struct handler {
  jmp_buf env;
  struct handler *next;
  value value;
  const char *type;
};

struct handler *u_push(void);
void u_pop(struct handler *h);
int u_has_type(struct handler *h, const char *type);
value u_caught(struct handler *h);
void u_raise(value v, const char *type);
void u_reraise(struct handler *h);

#endif
