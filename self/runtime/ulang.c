#include <gc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ulang.h"

static void *gc_realloc(void *p, size_t old, size_t size) {
  (void)old;
  return GC_REALLOC(p, size);
}

static void gc_free(void *p, size_t size) {
  (void)p;
  (void)size;
}

// the collector also manages the memory of GMP
__attribute__((constructor)) static void init(void) {
  GC_INIT();
  mp_set_memory_functions(GC_malloc, gc_realloc, gc_free);
}

static value object(enum kind kind) {
  value v = GC_MALLOC(sizeof(struct object));
  if (!v) fail("out of memory");
  v->kind = kind;
  return v;
}

value mk_int(long long n) {
  value v = object(INT);
  mpz_init_set_si(v->big, n);
  return v;
}

value mk_int_str(const char *digits) {
  value v = object(INT);
  mpz_init_set_str(v->big, digits, 10);
  return v;
}

static value big(void) {
  value v = object(INT);
  mpz_init(v->big);
  return v;
}

value mk_bool(int b) {
  value v = object(BOOL);
  v->num = b != 0;
  return v;
}

value mk_char(long long c) {
  value v = object(CHAR);
  v->num = c;
  return v;
}

value mk_str(const char *s) {
  value v = object(STR);
  v->text = s;
  return v;
}

static value *items(int size, va_list args) {
  value *xs = GC_MALLOC((size > 0 ? size : 1) * sizeof(value));
  for (int i = 0; i < size; i++) xs[i] = va_arg(args, value);
  return xs;
}

value mk_con(const char *name, int size, ...) {
  va_list args;
  va_start(args, size);
  value v = object(CON);
  v->text = name;
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

value mk_con_array(const char *name, int size, value *xs) {
  value v = object(CON);
  v->text = name;
  v->size = size;
  v->items = GC_MALLOC((size > 0 ? size : 1) * sizeof(value));
  for (int i = 0; i < size; i++) v->items[i] = xs[i];
  return v;
}

value mk_tuple(int size, ...) {
  va_list args;
  va_start(args, size);
  value v = object(TUPLE);
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

value mk_closure(code fun, int size, ...) {
  va_list args;
  va_start(args, size);
  value v = object(CLOSURE);
  v->fun = fun;
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

int truth(value v) {
  return v->num != 0;
}

int is_con(value v, const char *name) {
  return v->kind == CON && strcmp(v->text, name) == 0;
}

value field(value v, int i) {
  return v->items[i];
}

int equal(value a, value b) {
  if (a->kind == CLOSURE || b->kind == CLOSURE) fail("cannot compare functions");
  if (a->kind != b->kind) return 0;
  switch (a->kind) {
  case INT:
    return mpz_cmp(a->big, b->big) == 0;
  case BOOL:
  case CHAR:
    return a->num == b->num;
  case STR:
    return strcmp(a->text, b->text) == 0;
  case CON:
    if (strcmp(a->text, b->text) != 0) return 0;
    /* fall through */
  case TUPLE:
    if (a->size != b->size) return 0;
    for (int i = 0; i < a->size; i++)
      if (!equal(a->items[i], b->items[i])) return 0;
    return 1;
  default:
    return 0;
  }
}

value apply(value f, int n, ...) {
  va_list args;
  va_start(args, n);
  value *xs = items(n, args);
  va_end(args);
  return f->fun(f->items, xs);
}

value u_add(value a, value b) {
  value v = big();
  mpz_add(v->big, a->big, b->big);
  return v;
}

value u_sub(value a, value b) {
  value v = big();
  mpz_sub(v->big, a->big, b->big);
  return v;
}

value u_mul(value a, value b) {
  value v = big();
  mpz_mul(v->big, a->big, b->big);
  return v;
}

// division truncates toward zero, as BigInt in Scala
value u_div(value a, value b) {
  if (mpz_sgn(b->big) == 0) fail("division by zero");
  value v = big();
  mpz_tdiv_q(v->big, a->big, b->big);
  return v;
}

value u_mod(value a, value b) {
  if (mpz_sgn(b->big) == 0) fail("division by zero");
  value v = big();
  mpz_tdiv_r(v->big, a->big, b->big);
  return v;
}

value u_lt(value a, value b) {
  return mk_bool(mpz_cmp(a->big, b->big) < 0);
}

value u_eq(value a, value b) {
  return mk_bool(equal(a, b));
}

value u_chars(value s) {
  value cs = mk_con("Nil", 0);
  for (long i = (long)strlen(s->text) - 1; i >= 0; i--) cs = mk_con("Cons", 2, mk_char((unsigned char)s->text[i]), cs);
  return cs;
}

value u_string(value cs) {
  long n = 0;
  for (value c = cs; is_con(c, "Cons"); c = c->items[1]) n++;
  char *s = GC_MALLOC_ATOMIC(n + 1);
  long i = 0;
  for (value c = cs; is_con(c, "Cons"); c = c->items[1]) s[i++] = (char)c->items[0]->num;
  s[n] = 0;
  return mk_str(s);
}

value u_ord(value c) {
  return mk_int(c->num);
}

value u_chr(value n) {
  if (mpz_cmp_si(n->big, 0) < 0 || mpz_cmp_si(n->big, 65535) > 0) fail("no character");
  return mk_char(mpz_get_si(n->big));
}

value u_reverse(value xs) {
  value ys = mk_con("Nil", 0);
  for (; is_con(xs, "Cons"); xs = xs->items[1]) ys = mk_con("Cons", 2, xs->items[0], ys);
  return ys;
}

static char *text(size_t n) {
  char *s = GC_MALLOC_ATOMIC(n + 1);
  s[n] = 0;
  return s;
}

value u_concat(value s, value t) {
  size_t m = strlen(s->text), n = strlen(t->text);
  char *r = text(m + n);
  memcpy(r, s->text, m);
  memcpy(r + m, t->text, n);
  return mk_str(r);
}

value u_str_size(value s) {
  return mk_int((long long)strlen(s->text));
}

value u_str_join(value sep, value ss) {
  size_t n = 0, k = strlen(sep->text);
  for (value c = ss; is_con(c, "Cons"); c = c->items[1]) n += strlen(c->items[0]->text) + (c != ss ? k : 0);
  char *r = text(n), *p = r;
  for (value c = ss; is_con(c, "Cons"); c = c->items[1]) {
    if (c != ss) {
      memcpy(p, sep->text, k);
      p += k;
    }
    size_t m = strlen(c->items[0]->text);
    memcpy(p, c->items[0]->text, m);
    p += m;
  }
  return mk_str(r);
}

// from i up to but excluding j, as drop(i) and take(j - i) in Scala, which ignore indices outside of s
value u_str_slice(value s, value i, value j) {
  long n = (long)strlen(s->text), a = mpz_get_si(i->big), b = mpz_get_si(j->big);
  long start = a < 0 ? 0 : (a > n ? n : a), count = b - a;
  if (count < 0) count = 0;
  if (start + count > n) count = n - start;
  char *r = text(count);
  memcpy(r, s->text + start, count);
  return mk_str(r);
}

// the parts between the occurrences of c, including empty ones
value u_str_split(value s, value c) {
  value parts = mk_con("Nil", 0);
  const char *end = s->text + strlen(s->text), *from = end;
  for (const char *p = end; p >= s->text; p--) {
    if (p == s->text || p[-1] == (char)c->num) {
      char *r = text(from - p);
      memcpy(r, p, from - p);
      parts = mk_con("Cons", 2, mk_str(r), parts);
      from = p - 1;
    }
  }
  return parts;
}

value u_str_show(value n) {
  return mk_str(mpz_get_str(NULL, 10, n->big));
}

value u_str_number(value s) {
  const char *p = s->text + (s->text[0] == '-');
  if (!*p) return mk_con("None", 0);
  for (; *p; p++)
    if (*p < '0' || *p > '9') return mk_con("None", 0);
  return mk_con("Some", 1, mk_int_str(s->text));
}

value u_is_digit(value c) {
  return mk_bool('0' <= c->num && c->num <= '9');
}

value u_is_letter(value c) {
  return mk_bool(('a' <= c->num && c->num <= 'z') || ('A' <= c->num && c->num <= 'Z'));
}

value u_is_space(value c) {
  return mk_bool(c->num == ' ' || c->num == '\n' || c->num == '\t' || c->num == '\r');
}

// the escapes of the parser and the printer of ulang
static void print_char(FILE *out, long long c, char quote) {
  if (c == '\\') fprintf(out, "\\\\");
  else if (c == '\n') fprintf(out, "\\n");
  else if (c == '\t') fprintf(out, "\\t");
  else if (c == '\r') fprintf(out, "\\r");
  else if (c == quote) fprintf(out, "\\%c", quote);
  else fputc((int)c, out);
}

static int is_list(value v) {
  for (; is_con(v, "Cons"); v = v->items[1]);
  return is_con(v, "Nil");
}

// values as ulang/src/ulang/Printer.scala shows them
void fprint_value(FILE *out, value v) {
  switch (v->kind) {
  case INT:
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
      fprint_value(out, v->items[i]);
    }
    fputc(')', out);
    break;
  case CON:
    if (is_con(v, "Cons") && is_list(v)) {
      fputc('[', out);
      for (value x = v; is_con(x, "Cons"); x = x->items[1]) {
        if (x != v) fprintf(out, ", ");
        fprint_value(out, x->items[0]);
      }
      fputc(']', out);
    } else {
      fprintf(out, "%s", v->text);
      if (v->size > 0) {
        fputc('(', out);
        for (int i = 0; i < v->size; i++) {
          if (i > 0) fprintf(out, ", ");
          fprint_value(out, v->items[i]);
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

void print_value(value v) {
  fprint_value(stdout, v);
}

void print_line(value v) {
  print_value(v);
  putchar('\n');
}

void print_binding(const char *name, value v) {
  if (!v) return;
  printf("%s = ", name);
  print_line(v);
}

void fail(const char *msg) {
  fflush(stdout);
  fprintf(stderr, "runtime error: %s\n", msg);
  exit(1);
}

void no_case(value v) {
  fflush(stdout);
  fprintf(stderr, "runtime error: no case matches ");
  fprint_value(stderr, v);
  fprintf(stderr, "\n");
  exit(1);
}

void assert_true(value v) {
  if (!truth(v)) fail("assertion failed");
}

void assert_equal(value a, value b) {
  if (!equal(a, b)) {
    fflush(stdout);
    fprintf(stderr, "runtime error: assertion failed: ");
    fprint_value(stderr, a);
    fprintf(stderr, " != ");
    fprint_value(stderr, b);
    fprintf(stderr, "\n");
    exit(1);
  }
}

static struct handler *handlers = NULL;

struct handler *u_push(void) {
  struct handler *h = GC_MALLOC(sizeof(struct handler));
  h->next = handlers;
  handlers = h;
  return h;
}

void u_pop(struct handler *h) {
  handlers = h->next;
}

int u_has_type(struct handler *h, const char *type) {
  return strcmp(h->type, type) == 0;
}

value u_caught(struct handler *h) {
  return h->value;
}

// exceptions without a handler end the program as runtime errors
void u_raise(value v, const char *type) {
  if (!handlers) {
    fflush(stdout);
    fprintf(stderr, "runtime error: uncaught exception ");
    fprint_value(stderr, v);
    fprintf(stderr, "\n");
    exit(1);
  }
  handlers->value = v;
  handlers->type = type;
  longjmp(handlers->env, 1);
}

void u_reraise(struct handler *h) {
  u_raise(h->value, h->type);
}
