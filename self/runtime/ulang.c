#define _POSIX_C_SOURCE 200809L

#include <gc.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "ulang.h"

// the functions of libc on strings and memory, which are loops of the runtime when it is built with -DULANG_PROFILE,
// such that gprof attributes their time
#ifdef ULANG_PROFILE
__attribute__((noinline)) static size_t u_strlen(const char *s) {
  size_t n = 0;
  while (s[n]) n++;
  return n;
}

__attribute__((noinline)) static int u_strcmp(const char *s, const char *t) {
  while (*s && *s == *t) s++, t++;
  return (unsigned char)*s - (unsigned char)*t;
}

__attribute__((noinline)) static void *u_memcpy(void *d, const void *s, size_t n) {
  char *p = d;
  const char *q = s;
  while (n--) *p++ = *q++;
  return d;
}
#else
#define u_strlen strlen
#define u_strcmp strcmp
#define u_memcpy memcpy
#endif

static void *gc_realloc(void *p, size_t old, size_t size) {
  (void)old;
  return GC_REALLOC(p, size);
}

static void gc_free(void *p, size_t size) {
  (void)p;
  (void)size;
}

// the small integers, which exist once, since most integers of programs are small
#define SMALL_MIN (-16)
#define SMALL_MAX 255
static struct u_object small_ints[SMALL_MAX - SMALL_MIN + 1];

// the collector also manages the memory of GMP
__attribute__((constructor)) static void init(void) {
  GC_INIT();
  mp_set_memory_functions(GC_malloc, gc_realloc, gc_free);
  for (int i = 0; i <= SMALL_MAX - SMALL_MIN; i++) {
    small_ints[i].kind = INT;
    small_ints[i].num = i + SMALL_MIN;
  }
}

static u_value object(enum kind kind) {
  u_value v = GC_MALLOC(sizeof(struct u_object));
  if (!v) u_fail("out of memory");
  v->kind = kind;
  return v;
}

u_value u_mk_int(long long n) {
  if (n >= SMALL_MIN && n <= SMALL_MAX) return &small_ints[n - SMALL_MIN];
  u_value v = object(INT);
  v->num = n;
  return v;
}

static u_value big(void) {
  u_value v = object(BIG);
  mpz_init(v->big);
  return v;
}

// v as INT if it fits, which keeps one kind per value
static u_value normalize(u_value v) {
  return mpz_fits_slong_p(v->big) ? u_mk_int(mpz_get_si(v->big)) : v;
}

u_value u_mk_int_str(const char *digits) {
  u_value v = big();
  mpz_set_str(v->big, digits, 10);
  return normalize(v);
}

static void to_mpz(mpz_t z, u_value v) {
  if (v->kind == INT) mpz_set_si(z, v->num);
  else mpz_set(z, v->big);
}

// the result of the operation of GMP on a and b, for integers that are big or whose result may be
static u_value slow(void (*op)(mpz_ptr, mpz_srcptr, mpz_srcptr), u_value a, u_value b) {
  mpz_t x, y;
  mpz_init(x);
  mpz_init(y);
  to_mpz(x, a);
  to_mpz(y, b);
  u_value v = big();
  op(v->big, x, y);
  return normalize(v);
}

// -1, 0, or 1 as a is less than, equal to, or greater than b
static int compare_ints(u_value a, u_value b) {
  int c;
  if (a->kind == INT && b->kind == INT) return (a->num > b->num) - (a->num < b->num);
  if (a->kind == INT) c = -mpz_cmp_si(b->big, a->num);
  else if (b->kind == INT) c = mpz_cmp_si(a->big, b->num);
  else c = mpz_cmp(a->big, b->big);
  return (c > 0) - (c < 0);
}

// the decimal digits of an integer
static char *digits(u_value v) {
  if (v->kind == BIG) return mpz_get_str(NULL, 10, v->big);
  char *s = GC_MALLOC_ATOMIC(24);
  snprintf(s, 24, "%lld", v->num);
  return s;
}

// an integer as an index of a string, where big integers are beyond any string
static long long index_of(u_value v) {
  if (v->kind == INT) return v->num;
  return mpz_sgn(v->big) < 0 ? LLONG_MIN / 4 : LLONG_MAX / 4;
}

struct u_object u_true = {.kind = BOOL, .num = 1};
struct u_object u_false = {.kind = BOOL, .num = 0};
struct u_object u_nil = {.kind = CON, .name = &n_list_46_Nil, .size = 0, .items = NULL};

u_value u_mk_bool(int b) {
  return b ? &u_true : &u_false;
}

u_value u_mk_char(long long c) {
  u_value v = object(CHAR);
  v->num = c;
  return v;
}

// a string of n characters, which the runtime knows when it builds the string
u_value u_mk_str_n(const char *s, long long n) {
  u_value v = object(STR);
  v->text = s;
  v->length = n;
  return v;
}

// a string of C, e.g. a literal, whose length is measured once
u_value u_mk_str(const char *s) {
  return u_mk_str_n(s, (long long)u_strlen(s));
}

static u_value *items(int size, va_list args) {
  u_value *xs = GC_MALLOC((size > 0 ? size : 1) * sizeof(u_value));
  for (int i = 0; i < size; i++) xs[i] = va_arg(args, u_value);
  return xs;
}

const struct u_name n_list_46_Nil = {"list", "Nil", 0};
const struct u_name n_list_46_Cons = {"list", "Cons", 1};
const struct u_name n_option_46_None = {"option", "None", 0};
const struct u_name n_option_46_Some = {"option", "Some", 1};
const struct u_name n_os_46_OsError = {"os", "OsError", 0};

u_value u_mk_con(const struct u_name *name, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = object(CON);
  v->name = name;
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

u_value u_mk_con_array(const struct u_name *name, int size, u_value *xs) {
  u_value v = object(CON);
  v->name = name;
  v->size = size;
  v->items = GC_MALLOC((size > 0 ? size : 1) * sizeof(u_value));
  for (int i = 0; i < size; i++) v->items[i] = xs[i];
  return v;
}

u_value u_mk_tuple(int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = object(TUPLE);
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

u_value u_mk_closure(code fun, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = object(CLOSURE);
  v->fun = fun;
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

static int sign(long long d) {
  return (d > 0) - (d < 0);
}

// -1, 0, or 1 for values of the same type, where constructors are ordered by their declaration and then by their fields
static int compare(u_value a, u_value b) {
  if (a->kind == CLOSURE || b->kind == CLOSURE) u_fail("cannot compare functions");
  if (a == b) return 0;
  switch (a->kind) {
  case INT:
  case BIG:
    return compare_ints(a, b);
  case BOOL:
  case CHAR:
    return sign(a->num - b->num);
  case STR:
    return sign(u_strcmp(a->text, b->text));
  case CON:
    // descriptors of compilers before positions all have position 0, whose constructors still differ by name
    if (a->name != b->name) {
      int c = sign(a->name->index - b->name->index);
      return c != 0 ? c : sign(u_strcmp(a->name->base, b->name->base));
    }
    /* fall through */
  case TUPLE:
    for (int i = 0; i < a->size && i < b->size; i++) {
      int c = compare(a->items[i], b->items[i]);
      if (c != 0) return c;
    }
    return sign(a->size - b->size);
  default:
    return 0;
  }
}

int u_equals(u_value a, u_value b) {
  return compare(a, b) == 0;
}

u_value u_compare(u_value a, u_value b) {
  return u_mk_int(compare(a, b));
}

u_value u_apply(u_value f, int n, ...) {
  va_list args;
  va_start(args, n);
  u_value *xs = items(n, args);
  va_end(args);
  return f->fun(f->items, xs);
}

u_value u_add(u_value a, u_value b) {
  long long r;
  if (a->kind == INT && b->kind == INT && !__builtin_add_overflow(a->num, b->num, &r)) return u_mk_int(r);
  return slow(mpz_add, a, b);
}

u_value u_sub(u_value a, u_value b) {
  long long r;
  if (a->kind == INT && b->kind == INT && !__builtin_sub_overflow(a->num, b->num, &r)) return u_mk_int(r);
  return slow(mpz_sub, a, b);
}

u_value u_mul(u_value a, u_value b) {
  long long r;
  if (a->kind == INT && b->kind == INT && !__builtin_mul_overflow(a->num, b->num, &r)) return u_mk_int(r);
  return slow(mpz_mul, a, b);
}

// small integers that C divides, where the quotient of LLONG_MIN and -1 overflows
static int divisible(u_value a, u_value b) {
  return a->kind == INT && b->kind == INT && !(a->num == LLONG_MIN && b->num == -1);
}

// division truncates toward zero, as BigInt in Scala and C, and a big integer is never zero
u_value u_div(u_value a, u_value b) {
  if (b->kind == INT && b->num == 0) u_fail("division by zero");
  if (divisible(a, b)) return u_mk_int(a->num / b->num);
  return slow(mpz_tdiv_q, a, b);
}

u_value u_mod(u_value a, u_value b) {
  if (b->kind == INT && b->num == 0) u_fail("division by zero");
  if (divisible(a, b)) return u_mk_int(a->num % b->num);
  return slow(mpz_tdiv_r, a, b);
}

u_value u_lt(u_value a, u_value b) {
  return u_mk_bool(compare_ints(a, b) < 0);
}

u_value u_eq(u_value a, u_value b) {
  return u_mk_bool(u_equals(a, b));
}

u_value u_chars(u_value s) {
  u_value cs = &u_nil;
  for (long i = (long)(size_t)s->length - 1; i >= 0; i--) cs = u_mk_con(&n_list_46_Cons, 2, u_mk_char((unsigned char)s->text[i]), cs);
  return cs;
}

u_value u_string(u_value cs) {
  long n = 0;
  for (u_value c = cs; u_is_con(c, &n_list_46_Cons); c = c->items[1]) n++;
  char *s = GC_MALLOC_ATOMIC(n + 1);
  long i = 0;
  for (u_value c = cs; u_is_con(c, &n_list_46_Cons); c = c->items[1]) s[i++] = (char)c->items[0]->num;
  s[n] = 0;
  return u_mk_str_n(s, n);
}

u_value u_ord(u_value c) {
  return u_mk_int(c->num);
}

u_value u_chr(u_value n) {
  if (n->kind != INT || n->num < 0 || n->num > 65535) u_fail("no character");
  return u_mk_char(n->num);
}

// a cell with x at the place end of a list being built, whose tail is the place of the next cell,
// which changes the list only before it is visible to the program
u_value *u_append(u_value *end, u_value x) {
  *end = u_mk_con(&n_list_46_Cons, 2, x, &u_nil);
  return &(*end)->items[1];
}

static char *text(size_t n) {
  char *s = GC_MALLOC_ATOMIC(n + 1);
  s[n] = 0;
  return s;
}

// the escape of c in a string or character delimited by quote, as the parser reads it, or else NULL
static const char *escape(long long c, char quote) {
  if (c == '\\') return "\\\\";
  if (c == '\n') return "\\n";
  if (c == '\t') return "\\t";
  if (c == '\r') return "\\r";
  if (c == quote) return quote == '"' ? "\\\"" : "\\'";
  return NULL;
}

u_value u_concat(u_value s, u_value t) {
  size_t m = (size_t)s->length, n = (size_t)t->length;
  char *r = text(m + n);
  u_memcpy(r, s->text, m);
  u_memcpy(r + m, t->text, n);
  return u_mk_str_n(r, (long long)(m + n));
}

u_value u_str_size(u_value s) {
  return u_mk_int(s->length);
}

u_value u_str_join(u_value sep, u_value ss) {
  size_t n = 0, k = (size_t)sep->length;
  for (u_value c = ss; u_is_con(c, &n_list_46_Cons); c = c->items[1]) n += (size_t)c->items[0]->length + (c != ss ? k : 0);
  char *r = text(n), *p = r;
  for (u_value c = ss; u_is_con(c, &n_list_46_Cons); c = c->items[1]) {
    if (c != ss) {
      u_memcpy(p, sep->text, k);
      p += k;
    }
    size_t m = (size_t)c->items[0]->length;
    u_memcpy(p, c->items[0]->text, m);
    p += m;
  }
  return u_mk_str_n(r, (long long)n);
}

// from i up to but excluding j, as drop(i) and take(j - i) in Scala, which ignore indices outside of s
u_value u_str_slice(u_value s, u_value i, u_value j) {
  long long n = s->length, a = index_of(i), b = index_of(j);
  long long start = a < 0 ? 0 : (a > n ? n : a), count = b - a;
  if (count < 0) count = 0;
  if (start + count > n) count = n - start;
  char *r = text(count);
  u_memcpy(r, s->text + start, count);
  return u_mk_str_n(r, count);
}

// the parts between the occurrences of c, including empty ones
u_value u_str_split(u_value s, u_value c) {
  u_value parts = &u_nil;
  const char *end = s->text + (size_t)s->length, *from = end;
  for (const char *p = end; p >= s->text; p--) {
    if (p == s->text || p[-1] == (char)c->num) {
      char *r = text(from - p);
      u_memcpy(r, p, from - p);
      parts = u_mk_con(&n_list_46_Cons, 2, u_mk_str_n(r, from - p), parts);
      from = p - 1;
    }
  }
  return parts;
}

u_value u_str_get(u_value s, u_value i) {
  size_t n = (size_t)s->length;
  if (i->kind != INT || i->num < 0 || (size_t)i->num >= n) {
    char *d = digits(i);
    char *m = GC_MALLOC_ATOMIC(64 + u_strlen(d));
    sprintf(m, "no index %s in a string of size %zu", d, n);
    u_fail(m);
  }
  return u_mk_char((unsigned char)s->text[i->num]);
}

u_value u_str_contains(u_value s, u_value c) {
  return u_mk_bool(c->num != 0 && strchr(s->text, (int)c->num) != NULL);
}

u_value u_str_compare(u_value s, u_value t) {
  int c = u_strcmp(s->text, t->text);
  return u_mk_int(c < 0 ? -1 : c > 0);
}

u_value u_str_show(u_value n) {
  return u_mk_str(digits(n));
}

u_value u_str_quote(u_value s) {
  char *r = text(2 * (size_t)s->length + 2), *p = r;
  *p++ = '"';
  for (const char *c = s->text; *c; c++) {
    const char *e = escape(*c, '"');
    if (e) while (*e) *p++ = *e++;
    else *p++ = *c;
  }
  *p++ = '"';
  *p = 0;
  return u_mk_str_n(r, p - r);
}

u_value u_str_number(u_value s) {
  const char *p = s->text + (s->text[0] == '-');
  if (!*p) return u_mk_con(&n_option_46_None, 0);
  for (; *p; p++)
    if (*p < '0' || *p > '9') return u_mk_con(&n_option_46_None, 0);
  return u_mk_con(&n_option_46_Some, 1, u_mk_int_str(s->text));
}

u_value u_is_digit(u_value c) {
  return u_mk_bool('0' <= c->num && c->num <= '9');
}

u_value u_is_letter(u_value c) {
  return u_mk_bool(('a' <= c->num && c->num <= 'z') || ('A' <= c->num && c->num <= 'Z'));
}

u_value u_is_space(u_value c) {
  return u_mk_bool(c->num == ' ' || c->num == '\n' || c->num == '\t' || c->num == '\r');
}

static void print_char(FILE *out, long long c, char quote) {
  const char *e = escape(c, quote);
  if (e) fputs(e, out);
  else fputc((int)c, out);
}

static int is_list(u_value v) {
  for (; u_is_con(v, &n_list_46_Cons); v = v->items[1]);
  return u_is_con(v, &n_list_46_Nil);
}

// values as ulang/src/ulang/Printer.scala shows them
void u_fprint_value(FILE *out, u_value v) {
  switch (v->kind) {
  case INT:
    fprintf(out, "%lld", v->num);
    break;
  case BIG:
    mpz_out_str(out, 10, v->big);
    break;
  case BOOL:
    fprintf(out, v->num ? "true" : "false");
    break;
  case CHAR:
    fputc('\'', out);
    print_char(out, v->num, '\'');
    fputc('\'', out);
    break;
  case STR:
    fputc('"', out);
    for (const char *s = v->text; *s; s++) print_char(out, (unsigned char)*s, '"');
    fputc('"', out);
    break;
  case TUPLE:
    fputc('(', out);
    for (int i = 0; i < v->size; i++) {
      if (i > 0) fprintf(out, ", ");
      u_fprint_value(out, v->items[i]);
    }
    fputc(')', out);
    break;
  case CON:
    if (u_is_con(v, &n_list_46_Cons) && is_list(v)) {
      fputc('[', out);
      for (u_value x = v; u_is_con(x, &n_list_46_Cons); x = x->items[1]) {
        if (x != v) fprintf(out, ", ");
        u_fprint_value(out, x->items[0]);
      }
      fputc(']', out);
    } else {
      fprintf(out, "%s", v->name->base);
      if (v->size > 0) {
        fputc('(', out);
        for (int i = 0; i < v->size; i++) {
          if (i > 0) fprintf(out, ", ");
          u_fprint_value(out, v->items[i]);
        }
        fputc(')', out);
      }
    }
    break;
  case CLOSURE:
    fprintf(out, "<function>");
    break;
  }
}

void u_print_value(u_value v) {
  u_fprint_value(stdout, v);
}

void u_print_line(u_value v) {
  u_print_value(v);
  putchar('\n');
}

void u_print_binding(const char *name, u_value v) {
  if (!v) return;
  printf("%s = ", name);
  u_print_line(v);
}

void u_fail(const char *msg) {
  fflush(stdout);
  fprintf(stderr, "runtime error: %s\n", msg);
  exit(1);
}

void u_no_case(u_value v) {
  fflush(stdout);
  fprintf(stderr, "runtime error: no case matches ");
  u_fprint_value(stderr, v);
  fprintf(stderr, "\n");
  exit(1);
}

void u_assert_true(u_value v) {
  if (!u_truth(v)) u_fail("assertion failed");
}

void u_assert_equal(u_value a, u_value b) {
  if (!u_equals(a, b)) {
    fflush(stdout);
    fprintf(stderr, "runtime error: assertion failed: ");
    u_fprint_value(stderr, a);
    fprintf(stderr, " != ");
    u_fprint_value(stderr, b);
    fprintf(stderr, "\n");
    exit(1);
  }
}

static struct u_handler *handlers = NULL;

struct u_handler *u_push(void) {
  struct u_handler *h = GC_MALLOC(sizeof(struct u_handler));
  h->next = handlers;
  handlers = h;
  return h;
}

void u_pop(struct u_handler *h) {
  handlers = h->next;
}

int u_has_type(struct u_handler *h, const char *type) {
  return u_strcmp(h->type, type) == 0;
}

u_value u_caught(struct u_handler *h) {
  return h->value;
}

// exceptions without a handler end the program as runtime errors
void u_raise(u_value v, const char *type) {
  if (!handlers) {
    fflush(stdout);
    fprintf(stderr, "runtime error: uncaught exception ");
    u_fprint_value(stderr, v);
    fprintf(stderr, "\n");
    exit(1);
  }
  handlers->value = v;
  handlers->type = type;
  longjmp(handlers->env, 1);
}

void u_reraise(struct u_handler *h) {
  u_raise(h->value, h->type);
}

// the directory of the modules of the library, which the build of a program can set with -DULANG_LIB=path
#ifndef ULANG_LIB
#define ULANG_LIB "ulang/resources"
#endif

static void os_error(const char *message, const char *arg) {
  u_raise(u_mk_con(&n_os_46_OsError, 1, u_concat(u_mk_str(message), u_mk_str(arg))), "os.OsError");
}

// the contents of a file, or else NULL
static u_value read_file(const char *path) {
  FILE *f = fopen(path, "rb");
  if (!f) return NULL;
  struct stat st;
  if (fstat(fileno(f), &st) || !S_ISREG(st.st_mode)) {
    fclose(f);
    return NULL;
  }
  char *s = text(st.st_size);
  size_t n = fread(s, 1, st.st_size, f);
  fclose(f);
  return n == (size_t)st.st_size ? u_mk_str_n(s, (long long)n) : NULL;
}

// the arguments of the program, after its name
static u_value args = NULL;

void u_args(int argc, char **argv) {
  args = &u_nil;
  for (int i = argc - 1; i >= 1; i--) args = u_mk_con(&n_list_46_Cons, 2, u_mk_str(argv[i]), args);
}

void p_os_args(u_value *result) {
  *result = args;
}

void p_os_read(u_value path, u_value *result) {
  u_value t = read_file(path->text);
  if (!t) os_error("cannot read ", path->text);
  *result = t;
}

// creates the missing directories of path, as the interpreter does
void p_os_write(u_value path, u_value contents) {
  char *dir = strcpy(text((size_t)path->length), path->text);
  for (char *p = strchr(dir + 1, '/'); p; p = strchr(p + 1, '/')) {
    *p = 0;
    mkdir(dir, 0777);
    *p = '/';
  }
  FILE *f = fopen(path->text, "wb");
  if (!f) os_error("cannot write ", path->text);
  size_t n = (size_t)contents->length;
  int ok = fwrite(contents->text, 1, n, f) == n;
  if (fclose(f) || !ok) os_error("cannot write ", path->text);
}

void p_os_library(u_value name, u_value *result) {
  char *path = text(u_strlen(ULANG_LIB) + (size_t)name->length + 3);
  sprintf(path, "%s/%s.u", ULANG_LIB, name->text);
  u_value t = read_file(path);
  if (!t) os_error("unknown module ", name->text);
  *result = t;
}

void p_read(u_value path, u_value *program) {
  u_fail("read is not available in compiled programs");
}

void p_library(u_value name, u_value *program) {
  u_fail("library is not available in compiled programs");
}
