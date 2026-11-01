// the runtime of compiled ulang programs, where every value is a pointer to an object on the heap,
// which the Boehm collector frees, integers are unbounded with GMP, and exceptions jump to handlers

#ifndef ULANG_H
#define ULANG_H

#include <gmp.h>
#include <setjmp.h>
#include <stddef.h>

typedef struct u_object *u_value;

// the code of a closure, with its captured values and its arguments
typedef u_value (*code)(u_value *env, u_value *args);

enum kind { INT, BOOL, CHAR, STR, CON, TUPLE, CLOSURE };

// the fields of the kind of an object, where constructors, tuples, and closures have items
struct u_object {
  enum kind kind;
  union {
    mpz_t big;
    long long num;
    const char *text;
    struct {
      union {
        const char *name;
        code fun;
      };
      int size;
      u_value *items;
    };
  };
};

u_value mk_int(long long n);
u_value mk_int_str(const char *digits);
u_value mk_bool(int b);
u_value mk_char(long long c);
u_value mk_str(const char *s);
u_value mk_con(const char *name, int size, ...);
u_value mk_con_array(const char *name, int size, u_value *items);
u_value mk_tuple(int size, ...);
u_value mk_closure(code fun, int size, ...);

int truth(u_value v);
int is_con(u_value v, const char *name);
u_value field(u_value v, int i);
int equal(u_value a, u_value b);
u_value apply(u_value f, int n, ...);

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
u_value u_str_show(u_value n);
u_value u_str_quote(u_value s);
u_value u_str_number(u_value s);
u_value u_is_digit(u_value c);
u_value u_is_letter(u_value c);
u_value u_is_space(u_value c);

void print_value(u_value v);
void print_line(u_value v);
void print_binding(const char *name, u_value v);
void no_case(u_value v);
void assert_true(u_value v);
void assert_equal(u_value a, u_value b);
void fail(const char *msg);

// the handlers of the trys in progress, innermost first, where a raise jumps to the innermost one
struct handler {
  jmp_buf env;
  struct handler *next;
  u_value value;
  const char *type;
};

void u_args(int argc, char **argv);
void p_os_args(u_value *args);
void p_os_read(u_value path, u_value *text);
void p_os_write(u_value path, u_value text);
void p_os_library(u_value name, u_value *text);

// read and library of the interpreter, which give the syntax of a program, are not available in compiled programs
void p_read(u_value path, u_value *program);
void p_library(u_value name, u_value *program);


struct handler *u_push(void);
void u_pop(struct handler *h);
int u_has_type(struct handler *h, const char *type);
u_value u_caught(struct handler *h);
void u_raise(u_value v, const char *type);
void u_reraise(struct handler *h);

#endif
