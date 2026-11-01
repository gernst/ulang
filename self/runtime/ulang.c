#define _POSIX_C_SOURCE 200809L

#include <gc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

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

static u_value object(enum kind kind) {
  u_value v = GC_MALLOC(sizeof(struct u_object));
  if (!v) fail("out of memory");
  v->kind = kind;
  return v;
}

u_value mk_int(long long n) {
  u_value v = object(INT);
  mpz_init_set_si(v->big, n);
  return v;
}

u_value mk_int_str(const char *digits) {
  u_value v = object(INT);
  mpz_init_set_str(v->big, digits, 10);
  return v;
}

static u_value big(void) {
  u_value v = object(INT);
  mpz_init(v->big);
  return v;
}

struct u_object u_true = {.kind = BOOL, .num = 1};
struct u_object u_false = {.kind = BOOL, .num = 0};
struct u_object u_nil = {.kind = CON, .name = &n_list_46_Nil, .size = 0, .items = NULL};

u_value mk_bool(int b) {
  return b ? &u_true : &u_false;
}

u_value mk_char(long long c) {
  u_value v = object(CHAR);
  v->num = c;
  return v;
}

u_value mk_str(const char *s) {
  u_value v = object(STR);
  v->text = s;
  return v;
}

static u_value *items(int size, va_list args) {
  u_value *xs = GC_MALLOC((size > 0 ? size : 1) * sizeof(u_value));
  for (int i = 0; i < size; i++) xs[i] = va_arg(args, u_value);
  return xs;
}

const struct u_name n_list_46_Nil = {"list", "Nil"};
const struct u_name n_list_46_Cons = {"list", "Cons"};
const struct u_name n_option_46_None = {"option", "None"};
const struct u_name n_option_46_Some = {"option", "Some"};
const struct u_name n_os_46_OsError = {"os", "OsError"};

u_value mk_con(const struct u_name *name, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = object(CON);
  v->name = name;
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

u_value mk_con_array(const struct u_name *name, int size, u_value *xs) {
  u_value v = object(CON);
  v->name = name;
  v->size = size;
  v->items = GC_MALLOC((size > 0 ? size : 1) * sizeof(u_value));
  for (int i = 0; i < size; i++) v->items[i] = xs[i];
  return v;
}

u_value mk_tuple(int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = object(TUPLE);
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

u_value mk_closure(code fun, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = object(CLOSURE);
  v->fun = fun;
  v->size = size;
  v->items = items(size, args);
  va_end(args);
  return v;
}

int equal(u_value a, u_value b) {
  if (a->kind == CLOSURE || b->kind == CLOSURE) fail("cannot compare functions");
  if (a == b) return 1;
  if (a->kind != b->kind) return 0;
  switch (a->kind) {
  case INT:
    return mpz_cmp(a->big, b->big) == 0;
  case BOOL:
  case CHAR:
    return a->num == b->num;
  case STR:
    return a->text == b->text || strcmp(a->text, b->text) == 0;
  case CON:
    if (a->name != b->name) return 0;
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

u_value apply(u_value f, int n, ...) {
  va_list args;
  va_start(args, n);
  u_value *xs = items(n, args);
  va_end(args);
  return f->fun(f->items, xs);
}

u_value u_add(u_value a, u_value b) {
  u_value v = big();
  mpz_add(v->big, a->big, b->big);
  return v;
}

u_value u_sub(u_value a, u_value b) {
  u_value v = big();
  mpz_sub(v->big, a->big, b->big);
  return v;
}

u_value u_mul(u_value a, u_value b) {
  u_value v = big();
  mpz_mul(v->big, a->big, b->big);
  return v;
}

// division truncates toward zero, as BigInt in Scala
u_value u_div(u_value a, u_value b) {
  if (mpz_sgn(b->big) == 0) fail("division by zero");
  u_value v = big();
  mpz_tdiv_q(v->big, a->big, b->big);
  return v;
}

u_value u_mod(u_value a, u_value b) {
  if (mpz_sgn(b->big) == 0) fail("division by zero");
  u_value v = big();
  mpz_tdiv_r(v->big, a->big, b->big);
  return v;
}

u_value u_lt(u_value a, u_value b) {
  return mk_bool(mpz_cmp(a->big, b->big) < 0);
}

u_value u_eq(u_value a, u_value b) {
  return mk_bool(equal(a, b));
}

u_value u_chars(u_value s) {
  u_value cs = &u_nil;
  for (long i = (long)strlen(s->text) - 1; i >= 0; i--) cs = mk_con(&n_list_46_Cons, 2, mk_char((unsigned char)s->text[i]), cs);
  return cs;
}

u_value u_string(u_value cs) {
  long n = 0;
  for (u_value c = cs; is_con(c, &n_list_46_Cons); c = c->items[1]) n++;
  char *s = GC_MALLOC_ATOMIC(n + 1);
  long i = 0;
  for (u_value c = cs; is_con(c, &n_list_46_Cons); c = c->items[1]) s[i++] = (char)c->items[0]->num;
  s[n] = 0;
  return mk_str(s);
}

u_value u_ord(u_value c) {
  return mk_int(c->num);
}

u_value u_chr(u_value n) {
  if (mpz_cmp_si(n->big, 0) < 0 || mpz_cmp_si(n->big, 65535) > 0) fail("no character");
  return mk_char(mpz_get_si(n->big));
}

// a cell with x at the place end of a list being built, whose tail is the place of the next cell,
// which changes the list only before it is visible to the program
u_value *u_append(u_value *end, u_value x) {
  *end = mk_con(&n_list_46_Cons, 2, x, &u_nil);
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
  size_t m = strlen(s->text), n = strlen(t->text);
  char *r = text(m + n);
  memcpy(r, s->text, m);
  memcpy(r + m, t->text, n);
  return mk_str(r);
}

u_value u_str_size(u_value s) {
  return mk_int((long long)strlen(s->text));
}

u_value u_str_join(u_value sep, u_value ss) {
  size_t n = 0, k = strlen(sep->text);
  for (u_value c = ss; is_con(c, &n_list_46_Cons); c = c->items[1]) n += strlen(c->items[0]->text) + (c != ss ? k : 0);
  char *r = text(n), *p = r;
  for (u_value c = ss; is_con(c, &n_list_46_Cons); c = c->items[1]) {
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
u_value u_str_slice(u_value s, u_value i, u_value j) {
  long n = (long)strlen(s->text), a = mpz_get_si(i->big), b = mpz_get_si(j->big);
  long start = a < 0 ? 0 : (a > n ? n : a), count = b - a;
  if (count < 0) count = 0;
  if (start + count > n) count = n - start;
  char *r = text(count);
  memcpy(r, s->text + start, count);
  return mk_str(r);
}

// the parts between the occurrences of c, including empty ones
u_value u_str_split(u_value s, u_value c) {
  u_value parts = &u_nil;
  const char *end = s->text + strlen(s->text), *from = end;
  for (const char *p = end; p >= s->text; p--) {
    if (p == s->text || p[-1] == (char)c->num) {
      char *r = text(from - p);
      memcpy(r, p, from - p);
      parts = mk_con(&n_list_46_Cons, 2, mk_str(r), parts);
      from = p - 1;
    }
  }
  return parts;
}

u_value u_str_get(u_value s, u_value i) {
  size_t n = strlen(s->text);
  if (mpz_sgn(i->big) < 0 || mpz_cmp_ui(i->big, n) >= 0) {
    char *m = GC_MALLOC_ATOMIC(64 + mpz_sizeinbase(i->big, 10));
    sprintf(m, "no index %s in a string of size %zu", mpz_get_str(NULL, 10, i->big), n);
    fail(m);
  }
  return mk_char((unsigned char)s->text[mpz_get_ui(i->big)]);
}

u_value u_str_contains(u_value s, u_value c) {
  return mk_bool(c->num != 0 && strchr(s->text, (int)c->num) != NULL);
}

u_value u_str_show(u_value n) {
  return mk_str(mpz_get_str(NULL, 10, n->big));
}

u_value u_str_quote(u_value s) {
  char *r = text(2 * strlen(s->text) + 2), *p = r;
  *p++ = '"';
  for (const char *c = s->text; *c; c++) {
    const char *e = escape(*c, '"');
    if (e) while (*e) *p++ = *e++;
    else *p++ = *c;
  }
  *p++ = '"';
  *p = 0;
  return mk_str(r);
}

u_value u_str_number(u_value s) {
  const char *p = s->text + (s->text[0] == '-');
  if (!*p) return mk_con(&n_option_46_None, 0);
  for (; *p; p++)
    if (*p < '0' || *p > '9') return mk_con(&n_option_46_None, 0);
  return mk_con(&n_option_46_Some, 1, mk_int_str(s->text));
}

u_value u_is_digit(u_value c) {
  return mk_bool('0' <= c->num && c->num <= '9');
}

u_value u_is_letter(u_value c) {
  return mk_bool(('a' <= c->num && c->num <= 'z') || ('A' <= c->num && c->num <= 'Z'));
}

u_value u_is_space(u_value c) {
  return mk_bool(c->num == ' ' || c->num == '\n' || c->num == '\t' || c->num == '\r');
}

static void print_char(FILE *out, long long c, char quote) {
  const char *e = escape(c, quote);
  if (e) fputs(e, out);
  else fputc((int)c, out);
}

static int is_list(u_value v) {
  for (; is_con(v, &n_list_46_Cons); v = v->items[1]);
  return is_con(v, &n_list_46_Nil);
}

// values as ulang/src/ulang/Printer.scala shows them
void fprint_value(FILE *out, u_value v) {
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
    if (is_con(v, &n_list_46_Cons) && is_list(v)) {
      fputc('[', out);
      for (u_value x = v; is_con(x, &n_list_46_Cons); x = x->items[1]) {
        if (x != v) fprintf(out, ", ");
        fprint_value(out, x->items[0]);
      }
      fputc(']', out);
    } else {
      fprintf(out, "%s", v->name->base);
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

void print_value(u_value v) {
  fprint_value(stdout, v);
}

void print_line(u_value v) {
  print_value(v);
  putchar('\n');
}

void print_binding(const char *name, u_value v) {
  if (!v) return;
  printf("%s = ", name);
  print_line(v);
}

void fail(const char *msg) {
  fflush(stdout);
  fprintf(stderr, "runtime error: %s\n", msg);
  exit(1);
}

void no_case(u_value v) {
  fflush(stdout);
  fprintf(stderr, "runtime error: no case matches ");
  fprint_value(stderr, v);
  fprintf(stderr, "\n");
  exit(1);
}

void assert_true(u_value v) {
  if (!truth(v)) fail("assertion failed");
}

void assert_equal(u_value a, u_value b) {
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

u_value u_caught(struct handler *h) {
  return h->value;
}

// exceptions without a handler end the program as runtime errors
void u_raise(u_value v, const char *type) {
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

// the directory of the modules of the library, which the build of a program can set with -DULANG_LIB=path
#ifndef ULANG_LIB
#define ULANG_LIB "ulang/resources"
#endif

static void os_error(const char *message, const char *arg) {
  u_raise(mk_con(&n_os_46_OsError, 1, u_concat(mk_str(message), mk_str(arg))), "os.OsError");
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
  return n == (size_t)st.st_size ? mk_str(s) : NULL;
}

// the arguments of the program, after its name
static u_value args = NULL;

void u_args(int argc, char **argv) {
  args = &u_nil;
  for (int i = argc - 1; i >= 1; i--) args = mk_con(&n_list_46_Cons, 2, mk_str(argv[i]), args);
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
  char *dir = strcpy(text(strlen(path->text)), path->text);
  for (char *p = strchr(dir + 1, '/'); p; p = strchr(p + 1, '/')) {
    *p = 0;
    mkdir(dir, 0777);
    *p = '/';
  }
  FILE *f = fopen(path->text, "wb");
  if (!f) os_error("cannot write ", path->text);
  size_t n = strlen(contents->text);
  int ok = fwrite(contents->text, 1, n, f) == n;
  if (fclose(f) || !ok) os_error("cannot write ", path->text);
}

void p_os_library(u_value name, u_value *result) {
  char *path = text(strlen(ULANG_LIB) + strlen(name->text) + 3);
  sprintf(path, "%s/%s.u", ULANG_LIB, name->text);
  u_value t = read_file(path);
  if (!t) os_error("unknown module ", name->text);
  *result = t;
}

void p_read(u_value path, u_value *program) {
  fail("read is not available in compiled programs");
}

void p_library(u_value name, u_value *program) {
  fail("library is not available in compiled programs");
}
