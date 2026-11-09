#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <gc.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

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
static struct u_object small_chars[256];

// the collector also manages the memory of GMP
__attribute__((constructor)) static void init(void) {
  GC_INIT();
  // a small heap collects often while it grows, which took half the time of the compiler
  GC_expand_hp(128 << 20);
  mp_set_memory_functions(GC_malloc, gc_realloc, gc_free);
  for (int i = 0; i <= SMALL_MAX - SMALL_MIN; i++) {
    small_ints[i].type = &u_int_type;
    small_ints[i].num = i + SMALL_MIN;
  }
  for (int i = 0; i < 256; i++) {
    small_chars[i].type = &u_char_type;
    small_chars[i].num = i;
  }
}

static u_value object(const struct u_type *type) {
  u_value v = GC_MALLOC(sizeof(struct u_object));
  if (!v) u_fail("out of memory");
  v->type = type;
  return v;
}

u_value u_mk_int(long long n) {
  if (n >= SMALL_MIN && n <= SMALL_MAX) return &small_ints[n - SMALL_MIN];
  u_value v = object(&u_int_type);
  v->num = n;
  return v;
}

static u_value big(void) {
  u_value v = object(&u_big_type);
  mpz_init(v->big);
  return v;
}

// v as a small integer if it fits, which keeps one representation per integer
static u_value normalize(u_value v) {
  return mpz_fits_slong_p(v->big) ? u_mk_int(mpz_get_si(v->big)) : v;
}

u_value u_mk_int_str(const char *digits) {
  u_value v = big();
  mpz_set_str(v->big, digits, 10);
  return normalize(v);
}

static void to_mpz(mpz_t z, u_value v) {
  if (v->type == &u_int_type) mpz_set_si(z, v->num);
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
static int int_order(u_value a, u_value b) {
  int c;
  if (a->type == &u_int_type && b->type == &u_int_type) return (a->num > b->num) - (a->num < b->num);
  if (a->type == &u_int_type) c = -mpz_cmp_si(b->big, a->num);
  else if (b->type == &u_int_type) c = mpz_cmp_si(a->big, b->num);
  else c = mpz_cmp(a->big, b->big);
  return (c > 0) - (c < 0);
}

// the decimal digits of an integer
static char *digits(u_value v) {
  if (v->type == &u_big_type) return mpz_get_str(NULL, 10, v->big);
  char *s = GC_MALLOC_ATOMIC(24);
  snprintf(s, 24, "%lld", v->num);
  return s;
}

// an integer as an index of a string or an array, where big integers are beyond any
static long long index_of(u_value v) {
  if (v->type == &u_int_type) return v->num;
  return mpz_sgn(v->big) < 0 ? LLONG_MIN / 4 : LLONG_MAX / 4;
}

struct u_object u_true = {.type = &u_bool_type, .num = 1};
struct u_object u_false = {.type = &u_bool_type, .num = 0};

u_value u_mk_bool(int b) {
  return b ? &u_true : &u_false;
}

u_value u_mk_char(long long c) {
  if (c >= 0 && c < 256) return &small_chars[c];
  u_value v = object(&u_char_type);
  v->num = c;
  return v;
}

// the string of n code points stored as units of width bytes, which it shares, where the width is the narrowest
u_value u_mk_units(const void *units, long long n, int width) {
  u_value v = object(&u_str_type);
  v->units = units;
  v->length = n;
  v->width = width;
  return v;
}

// the string of a string of C whose bytes are code points below 128, e.g. digits
u_value u_mk_str(const char *s) {
  return u_mk_units(s, (long long)u_strlen(s), 1);
}

// the narrowest width of units that holds the code point c
static int width_of(long long c) {
  return c < 0x100 ? 1 : c < 0x10000 ? 2 : 4;
}

// a string of n code points of units of width bytes, which its builder fills by put_unit, followed by a 0
static u_value new_str(long long n, int width) {
  char *units = GC_MALLOC_ATOMIC((size_t)(n + 1) * width);
  if (!units) u_fail("out of memory");
  memset(units + n * width, 0, width);
  return u_mk_units(units, n, width);
}

static void put_unit(u_value s, long long i, long long c) {
  if (s->width == 1) ((unsigned char *)s->units)[i] = (unsigned char)c;
  else if (s->width == 2) ((uint16_t *)s->units)[i] = (uint16_t)c;
  else ((uint32_t *)s->units)[i] = (uint32_t)c;
}

// the n code points of s from index from into r from index to, where r is at least as wide as s
static void copy_units(u_value r, long long to, u_value s, long long from, long long n) {
  if (r->width == s->width) u_memcpy((char *)r->units + to * r->width, (const char *)s->units + from * s->width, n * s->width);
  else for (long long i = 0; i < n; i++) put_unit(r, to + i, u_str_at(s, from + i));
}

// the narrowest width of the code points of s from index i up to j
static int width_between(u_value s, long long i, long long j) {
  int w = 1;
  if (s->width == 1) return 1;
  for (; i < j && w < s->width; i++) {
    int v = width_of(u_str_at(s, i));
    if (v > w) w = v;
  }
  return w;
}

// the code points of s from index i up to j, as a string of the narrowest width
static u_value substring(u_value s, long long i, long long j) {
  u_value r = new_str(j - i, width_between(s, i, j));
  copy_units(r, 0, s, i, j - i);
  return r;
}

// the number of bytes of the UTF-8 of the code point c, and its UTF-8 at p
static int utf8_size(long long c) {
  return c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4;
}

static char *put_utf8(char *p, long long c) {
  if (c < 0x80) *p++ = (char)c;
  else if (c < 0x800) {
    *p++ = (char)(0xC0 | (c >> 6));
    *p++ = (char)(0x80 | (c & 0x3F));
  } else if (c < 0x10000) {
    *p++ = (char)(0xE0 | (c >> 12));
    *p++ = (char)(0x80 | ((c >> 6) & 0x3F));
    *p++ = (char)(0x80 | (c & 0x3F));
  } else {
    *p++ = (char)(0xF0 | (c >> 18));
    *p++ = (char)(0x80 | ((c >> 12) & 0x3F));
    *p++ = (char)(0x80 | ((c >> 6) & 0x3F));
    *p++ = (char)(0x80 | (c & 0x3F));
  }
  return p;
}

// the units of a string of code points below 128 are their UTF-8 already
const char *u_utf8(u_value s, size_t *n) {
  size_t m = 0;
  for (long long i = 0; i < s->length; i++) m += utf8_size(u_str_at(s, i));
  if (n) *n = m;
  if (s->width == 1 && m == (size_t)s->length) return s->units;
  char *r = GC_MALLOC_ATOMIC(m + 1), *p = r;
  if (!r) u_fail("out of memory");
  for (long long i = 0; i < s->length; i++) p = put_utf8(p, u_str_at(s, i));
  *p = 0;
  return r;
}

// the code point of the UTF-8 at p before end, with its number of bytes in k, or else -1, which excludes overlong
// encodings, surrogates, and code points beyond 0x10FFFF
static long long get_utf8(const unsigned char *p, const unsigned char *end, int *k) {
  long long c;
  int n;
  if (p[0] < 0x80) return *k = 1, p[0];
  if (p[0] >= 0xC2 && p[0] <= 0xDF) n = 2, c = p[0] & 0x1F;
  else if (p[0] >= 0xE0 && p[0] <= 0xEF) n = 3, c = p[0] & 0x0F;
  else if (p[0] >= 0xF0 && p[0] <= 0xF4) n = 4, c = p[0] & 0x07;
  else return -1;
  if (end - p < n) return -1;
  for (int i = 1; i < n; i++) {
    if ((p[i] & 0xC0) != 0x80) return -1;
    c = (c << 6) | (p[i] & 0x3F);
  }
  if ((n == 3 && c < 0x800) || (n == 4 && c < 0x10000) || c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) return -1;
  *k = n;
  return c;
}

// the string of the UTF-8 of n bytes, which it shares if they are code points below 128 followed by a 0 and owned,
// and else copies
static u_value decode(const char *bytes, size_t n, int owned) {
  const unsigned char *p = (const unsigned char *)bytes, *end = p + n;
  long long length = 0;
  int width = 1, k;
  for (const unsigned char *q = p; q < end; q += k, length++) {
    long long c = get_utf8(q, end, &k);
    if (c < 0) return NULL;
    if (width_of(c) > width) width = width_of(c);
  }
  if (owned && length == (long long)n) return u_mk_units(bytes, length, 1);
  u_value s = new_str(length, width);
  long long i = 0;
  for (const unsigned char *q = p; q < end; q += k) put_unit(s, i++, get_utf8(q, end, &k));
  return s;
}

u_value u_from_utf8(const char *bytes, size_t n) {
  return decode(bytes, n, 0);
}

// an object with its items after it, in one allocation
static u_value compound(const struct u_type *type, int size) {
  u_value v = GC_MALLOC(sizeof(struct u_object) + size * sizeof(u_value));
  if (!v) u_fail("out of memory");
  v->type = type;
  v->size = size;
  v->items = (u_value *)(v + 1);
  return v;
}

static u_value fill(u_value v, va_list args) {
  for (int i = 0; i < v->size; i++) v->items[i] = va_arg(args, u_value);
  return v;
}

// the error of the builtins on files, which programs need not import, whose descriptors are therefore defined here,
// while every program defines those of the datatypes of the prelude and its modules
const struct u_name n_os_46_OsError = {"os", "OsError", 0, &t_os_46_OsError};

static void division_by_zero(void) {
  u_raise(&o_prelude_46_DivisionByZero, "prelude.DivisionByZero");
}

static void out_of_bounds(u_value i, long long n) {
  u_raise(u_mk_con(&n_prelude_46_OutOfBounds, 2, i, u_mk_int(n)), "prelude.OutOfBounds");
}

u_value u_mk_con(const struct u_name *name, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = fill(compound(name->type, size), args);
  v->name = name;
  va_end(args);
  return v;
}

u_value u_mk_con_array(const struct u_name *name, int size, u_value *xs) {
  u_value v = compound(name->type, size);
  v->name = name;
  for (int i = 0; i < size; i++) v->items[i] = xs[i];
  return v;
}

u_value u_mk_tuple(int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = fill(compound(&u_tuple_type, size), args);
  va_end(args);
  return v;
}

u_value u_mk_closure(code fun, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = fill(compound(&u_closure_type, size), args);
  v->fun = fun;
  va_end(args);
  return v;
}

u_value u_mk_proc(proc_code proc, int size, ...) {
  va_list args;
  va_start(args, size);
  u_value v = fill(compound(&u_proc_type, size), args);
  v->proc = proc;
  va_end(args);
  return v;
}

static int sign(long long d) {
  return (d > 0) - (d < 0);
}

// -1, 0, or 1 as a value, which are shared
static u_value ordering(int c) {
  return &small_ints[c - SMALL_MIN];
}

// -1, 0, or 1 for values of the same type, by the order of their type
static int compare(u_value a, u_value b) {
  return (int)a->type->compare(a, b)->num;
}

int u_equals(u_value a, u_value b) {
  return u_truth(a->type->equals(a, b));
}

u_value u_equals_by_compare(u_value a, u_value b) {
  return u_mk_bool(compare(a, b) == 0);
}

static u_value compare_ints(u_value a, u_value b) {
  return ordering(int_order(a, b));
}

static u_value compare_nums(u_value a, u_value b) {
  return ordering(sign(a->num - b->num));
}

// strings by their code points and then by their length
static u_value compare_strs(u_value a, u_value b) {
  if (a == b) return ordering(0);
  long long n = a->length < b->length ? a->length : b->length;
  if (a->width == 1 && b->width == 1) {
    int c = memcmp(a->units, b->units, (size_t)n);
    if (c != 0) return ordering(sign(c));
  } else {
    for (long long i = 0; i < n; i++) {
      long long c = u_str_at(a, i), d = u_str_at(b, i);
      if (c != d) return ordering(c < d ? -1 : 1);
    }
  }
  return ordering(sign(a->length - b->length));
}

// strings of the same code points have the same width and units, since the width is the narrowest
static u_value equal_strs(u_value a, u_value b) {
  return u_mk_bool(a == b || (a->length == b->length && a->width == b->width &&
                              memcmp(a->units, b->units, (size_t)(a->length * a->width)) == 0));
}

static int compare_items(u_value a, u_value b) {
  for (int i = 0; i < a->size && i < b->size; i++) {
    int c = compare(a->items[i], b->items[i]);
    if (c != 0) return c;
  }
  return sign(a->size - b->size);
}

static u_value compare_tuples(u_value a, u_value b) {
  return ordering(a == b ? 0 : compare_items(a, b));
}

// constructors are ordered by their position in their declaration and then by their fields, where descriptors of
// compilers before positions all have position 0, whose constructors still differ by name
u_value u_con_compare(u_value a, u_value b) {
  if (a == b) return ordering(0);
  if (a->name != b->name) {
    int c = sign(a->name->index - b->name->index);
    return ordering(c != 0 ? c : sign(u_strcmp(a->name->base, b->name->base)));
  }
  return ordering(compare_items(a, b));
}

// arrays by their elements and then by their size, as lists
static u_value compare_arrays(u_value a, u_value b) {
  return ordering(a == b ? 0 : compare_items(a, b));
}

static void fail_with(const char *format, u_value a, u_value b);

static u_value compare_functions(u_value a, u_value b) {
  (void)a;
  (void)b;
  fail_with(u_text_compare_functions, NULL, NULL);
  return NULL;
}

static u_value compare_procedures(u_value a, u_value b) {
  (void)a;
  (void)b;
  fail_with(u_text_compare_procedures, NULL, NULL);
  return NULL;
}

u_value u_compare(u_value a, u_value b) {
  return a->type->compare(a, b);
}

u_value u_apply(u_value f, int n, ...) {
  va_list args;
  va_start(args, n);
  // on the stack, since closures copy their arguments
  u_value xs[n > 0 ? n : 1];
  for (int i = 0; i < n; i++) xs[i] = va_arg(args, u_value);
  va_end(args);
  return f->fun(f->items, xs);
}

// calls a procedure value with the values of its ins, followed by the pointers to its outs and refs
void u_call_proc(u_value p, int ins, int outs, ...) {
  va_list args;
  va_start(args, outs);
  u_value xs[ins > 0 ? ins : 1];
  u_value *ys[outs > 0 ? outs : 1];
  for (int i = 0; i < ins; i++) xs[i] = va_arg(args, u_value);
  for (int i = 0; i < outs; i++) ys[i] = va_arg(args, u_value *);
  va_end(args);
  p->proc(p->items, xs, ys);
}

u_value u_add(u_value a, u_value b) {
  long long r;
  if (a->type == &u_int_type && b->type == &u_int_type && !__builtin_add_overflow(a->num, b->num, &r)) return u_mk_int(r);
  return slow(mpz_add, a, b);
}

u_value u_sub(u_value a, u_value b) {
  long long r;
  if (a->type == &u_int_type && b->type == &u_int_type && !__builtin_sub_overflow(a->num, b->num, &r)) return u_mk_int(r);
  return slow(mpz_sub, a, b);
}

u_value u_mul(u_value a, u_value b) {
  long long r;
  if (a->type == &u_int_type && b->type == &u_int_type && !__builtin_mul_overflow(a->num, b->num, &r)) return u_mk_int(r);
  return slow(mpz_mul, a, b);
}

// small integers that C divides, where the quotient of LLONG_MIN and -1 overflows
static int divisible(u_value a, u_value b) {
  return a->type == &u_int_type && b->type == &u_int_type && !(a->num == LLONG_MIN && b->num == -1);
}

// division truncates toward zero, as in C, and a big integer is never zero
u_value u_div(u_value a, u_value b) {
  if (b->type == &u_int_type && b->num == 0) division_by_zero();
  if (divisible(a, b)) return u_mk_int(a->num / b->num);
  return slow(mpz_tdiv_q, a, b);
}

u_value u_mod(u_value a, u_value b) {
  if (b->type == &u_int_type && b->num == 0) division_by_zero();
  if (divisible(a, b)) return u_mk_int(a->num % b->num);
  return slow(mpz_tdiv_r, a, b);
}

// small integers directly, and other values by the order of their type
u_value u_lt(u_value a, u_value b) {
  if (a->type == &u_int_type && b->type == &u_int_type) return u_mk_bool(a->num < b->num);
  return u_mk_bool(compare(a, b) < 0);
}

u_value u_eq(u_value a, u_value b) {
  return a->type->equals(a, b);
}

u_value u_chars(u_value s) {
  u_value cs = &o_list_46_Nil;
  for (long long i = s->length - 1; i >= 0; i--) cs = u_mk_con(&n_list_46_Cons, 2, u_mk_char(u_str_at(s, i)), cs);
  return cs;
}

u_value u_string(u_value cs) {
  long long n = 0;
  int width = 1;
  for (u_value c = cs; u_is_con(c, &n_list_46_Cons); c = c->items[1], n++)
    if (width_of(c->items[0]->num) > width) width = width_of(c->items[0]->num);
  u_value s = new_str(n, width);
  long long i = 0;
  for (u_value c = cs; u_is_con(c, &n_list_46_Cons); c = c->items[1]) put_unit(s, i++, c->items[0]->num);
  return s;
}

u_value u_ord(u_value c) {
  return u_mk_int(c->num);
}

// the code points, without the surrogates of UTF-16
u_value u_chr(u_value n) {
  if (n->type != &u_int_type || n->num < 0 || n->num > 0x10FFFF || (n->num >= 0xD800 && n->num <= 0xDFFF))
    u_raise(u_mk_con(&n_prelude_46_NoCharacter, 1, n), "prelude.NoCharacter");
  return u_mk_char(n->num);
}

// a cell with x at the place end of a list being built, whose tail is the place of the next cell,
// which changes the list only before it is visible to the program
u_value *u_append(u_value *end, u_value x) {
  *end = u_mk_con(&n_list_46_Cons, 2, x, &o_list_46_Nil);
  return &(*end)->items[1];
}

static char *text(size_t n) {
  char *s = GC_MALLOC_ATOMIC(n + 1);
  s[n] = 0;
  return s;
}

// a string that grows by appending, for quote and the shows of values, as a string of units of the narrowest width
// of its code points so far
struct u_text {
  void *units;
  long long length, capacity;
  int width;
};

// room for n more code points of width bytes, where the units are copied when they widen
static void reserve(struct u_text *t, long long n, int width) {
  if (t->length + n + 1 <= t->capacity && width <= t->width) return;
  long long capacity = t->length + n + 1 <= t->capacity ? t->capacity : 2 * (t->length + n + 1);
  int w = width > t->width ? width : t->width;
  struct u_object old = {.units = t->units, .length = t->length, .width = t->width};
  u_value r = new_str(capacity - 1, w);
  if (t->units) copy_units(r, 0, &old, 0, t->length);
  t->units = (void *)r->units;
  t->capacity = capacity;
  t->width = w;
}

static void append_code(struct u_text *t, long long c) {
  reserve(t, 1, width_of(c));
  struct u_object r = {.units = t->units, .width = t->width};
  put_unit(&r, t->length++, c);
}

static void append(struct u_text *t, const char *s) {
  while (*s) append_code(t, (unsigned char)*s++);
}

static void append_str(struct u_text *t, u_value s) {
  reserve(t, s->length, s->width);
  struct u_object r = {.units = t->units, .width = t->width};
  copy_units(&r, t->length, s, 0, s->length);
  t->length += s->length;
}

static u_value text_value(struct u_text *t) {
  if (!t->units) return u_mk_lit("");
  memset((char *)t->units + t->length * t->width, 0, t->width);
  return u_mk_units(t->units, t->length, t->width);
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

// c, escaped if it needs to be in a string or character delimited by quote
static void append_char(struct u_text *t, long long c, char quote) {
  const char *e = escape(c, quote);
  if (e) append(t, e);
  else append_code(t, c);
}

// of the width of the wider string, which holds the widest code point
u_value u_concat(u_value s, u_value t) {
  u_value r = new_str(s->length + t->length, s->width > t->width ? s->width : t->width);
  copy_units(r, 0, s, 0, s->length);
  copy_units(r, s->length, t, 0, t->length);
  return r;
}

// the functions of the builtin section of str.u

u_value u_size(u_value s) {
  return u_mk_int(s->length);
}

u_value u_join(u_value sep, u_value ss) {
  long long n = 0;
  int width = 1;
  for (u_value c = ss; u_is_con(c, &n_list_46_Cons); c = c->items[1]) {
    u_value s = c != ss ? sep : NULL;
    n += c->items[0]->length + (s ? s->length : 0);
    if (c->items[0]->width > width) width = c->items[0]->width;
    if (s && s->width > width) width = s->width;
  }
  u_value r = new_str(n, width);
  long long i = 0;
  for (u_value c = ss; u_is_con(c, &n_list_46_Cons); c = c->items[1]) {
    if (c != ss) {
      copy_units(r, i, sep, 0, sep->length);
      i += sep->length;
    }
    copy_units(r, i, c->items[0], 0, c->items[0]->length);
    i += c->items[0]->length;
  }
  return r;
}

// from i up to but excluding j, where 0 <= i <= j <= the size of s, or else OutOfBounds
u_value u_slice(u_value s, u_value i, u_value j) {
  long long n = s->length, a = index_of(i), b = index_of(j);
  if (a < 0 || a > n || a > b) out_of_bounds(i, n);
  if (b > n) out_of_bounds(j, n);
  return substring(s, a, b);
}

// the parts between the occurrences of c, including empty ones
u_value u_split(u_value s, u_value c) {
  u_value parts = &o_list_46_Nil;
  long long to = s->length;
  for (long long i = s->length; i >= 0; i--) {
    if (i == 0 || u_str_at(s, i - 1) == c->num) {
      parts = u_mk_con(&n_list_46_Cons, 2, substring(s, i, to), parts);
      to = i - 1;
    }
  }
  return parts;
}

u_value u_at(u_value i, u_value s) {
  long long k = index_of(i);
  if (k < 0 || k >= s->length) out_of_bounds(i, s->length);
  return u_mk_char(u_str_at(s, k));
}

u_value u_contains(u_value c, u_value s) {
  if (s->width == 1) return u_mk_bool(c->num < 0x100 && memchr(s->units, (int)c->num, (size_t)s->length) != NULL);
  for (long long i = 0; i < s->length; i++)
    if (u_str_at(s, i) == c->num) return &u_true;
  return &u_false;
}

u_value u_quote(u_value s) {
  struct u_text t = {NULL, 0, 0, 1};
  append_char(&t, '"', 0);
  for (long long i = 0; i < s->length; i++) append_char(&t, u_str_at(s, i), '"');
  append_char(&t, '"', 0);
  return text_value(&t);
}

// digits are code points below 128, which a string of a wider width has none but
u_value u_number(u_value s) {
  if (s->width != 1) return &o_option_46_None;
  const char *text = s->units;
  long long i = text[0] == '-';
  if (i == s->length) return &o_option_46_None;
  for (; i < s->length; i++)
    if (text[i] < '0' || text[i] > '9') return &o_option_46_None;
  return u_mk_con(&n_option_46_Some, 1, u_mk_int_str(text));
}

// the functions of the builtin section of array.u

// the array of n elements, of a literal |a, b|
u_value u_mk_array(int n, ...) {
  va_list args;
  va_start(args, n);
  u_value a = fill(compound(&u_array_type, n), args);
  va_end(args);
  return a;
}

// the number of elements of a list or an array, the source of a comprehension
int u_count(u_value c) {
  if (c->type == &u_array_type) return c->size;
  int n = 0;
  for (; u_is_con(c, &n_list_46_Cons); c = c->items[1]) n++;
  return n;
}

// an array with room for n elements of a comprehension, which u_array_push fills, and which has no elements yet
u_value u_array_new(int n) {
  u_value a = compound(&u_array_type, n);
  a->size = 0;
  return a;
}

// a with x after its elements, which is a itself while the allocation of a has room, as the collector knows it, and else a
// copy with twice the room
u_value u_array_push(u_value a, u_value x) {
  size_t room = (GC_size(a) - sizeof(struct u_object)) / sizeof(u_value);
  if ((size_t)a->size == room) {
    u_value b = compound(&u_array_type, (int)(2 * room + 8));
    u_memcpy(b->items, a->items, a->size * sizeof(u_value));
    b->size = a->size;
    a = b;
  }
  a->items[a->size++] = x;
  return a;
}

// the array of the elements pushed into an array with room for n elements, which is a copy unless it has n elements, where n
// is -1 if the number of elements is unknown
u_value u_array_done(u_value a, int n) {
  if (a->size == n) return a;
  u_value b = compound(&u_array_type, a->size);
  u_memcpy(b->items, a->items, a->size * sizeof(u_value));
  return b;
}

u_value u_array(u_value xs) {
  int n = 0;
  for (u_value c = xs; u_is_con(c, &n_list_46_Cons); c = c->items[1]) n++;
  u_value a = compound(&u_array_type, n);
  int i = 0;
  for (u_value c = xs; u_is_con(c, &n_list_46_Cons); c = c->items[1]) a->items[i++] = c->items[0];
  return a;
}

u_value u_array_list(u_value a) {
  u_value xs = &o_list_46_Nil;
  for (int i = a->size - 1; i >= 0; i--) xs = u_mk_con(&n_list_46_Cons, 2, a->items[i], xs);
  return xs;
}

u_value u_array_size(u_value a) {
  return u_mk_int(a->size);
}

u_value u_array_at(u_value i, u_value a) {
  long long k = index_of(i);
  if (k < 0 || k >= a->size) out_of_bounds(i, a->size);
  return a->items[k];
}

u_value u_array_put(u_value i, u_value x, u_value a) {
  long long k = index_of(i);
  if (k < 0 || k >= a->size) out_of_bounds(i, a->size);
  u_value b = compound(&u_array_type, a->size);
  u_memcpy(b->items, a->items, a->size * sizeof(u_value));
  b->items[k] = x;
  return b;
}

// from i up to but excluding j, where 0 <= i <= j <= the size of a, or else OutOfBounds
u_value u_array_slice(u_value a, u_value i, u_value j) {
  long long n = a->size, k = index_of(i), l = index_of(j);
  if (k < 0 || k > n || k > l) out_of_bounds(i, n);
  if (l > n) out_of_bounds(j, n);
  u_value b = compound(&u_array_type, (int)(l - k));
  u_memcpy(b->items, a->items + k, (l - k) * sizeof(u_value));
  return b;
}

u_value u_array_foldr(u_value a, u_value acc, u_value f) {
  for (int i = a->size - 1; i >= 0; i--) acc = u_apply(f, 2, a->items[i], acc);
  return acc;
}

u_value u_array_foldl(u_value a, u_value acc, u_value f) {
  for (int i = 0; i < a->size; i++) acc = u_apply(f, 2, acc, a->items[i]);
  return acc;
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

// the text of a value, shown by its type, appended
static void append_shown(struct u_text *t, u_value v) {
  append_str(t, u_show(v));
}

// values as self/print.u shows them, by their types
u_value u_show(u_value v) {
  return v->type->show(v);
}

void u_fprint_value(FILE *out, u_value v) {
  size_t n;
  const char *s = u_utf8(u_show(v), &n);
  fwrite(s, 1, n, out);
}

static u_value show_int(u_value v) {
  return u_mk_str(digits(v));
}

static u_value show_bool(u_value v) {
  return v->num ? u_mk_lit("true") : u_mk_lit("false");
}

static u_value show_char(u_value v) {
  struct u_text t = {NULL, 0, 0, 1};
  append(&t, "'");
  append_char(&t, v->num, '\'');
  append(&t, "'");
  return text_value(&t);
}

static u_value show_str(u_value v) {
  return u_quote(v);
}

static void append_items(struct u_text *t, u_value v) {
  append(t, "(");
  for (int i = 0; i < v->size; i++) {
    if (i > 0) append(t, ", ");
    append_shown(t, v->items[i]);
  }
  append(t, ")");
}

static u_value show_tuple(u_value v) {
  struct u_text t = {NULL, 0, 0, 1};
  append_items(&t, v);
  return text_value(&t);
}

// constructors with their fields, where the library declares the shows of lists, options, sets, and maps
u_value u_con_show(u_value v) {
  struct u_text t = {NULL, 0, 0, 1};
  append(&t, v->name->base);
  if (v->size > 0) append_items(&t, v);
  return text_value(&t);
}

static u_value show_array(u_value v) {
  struct u_text t = {NULL, 0, 0, 1};
  append(&t, "|");
  for (int i = 0; i < v->size; i++) {
    u_value s = u_show(v->items[i]);
    // a first element that starts with || in parentheses, since the parser reads ||| as an empty array before a |
    int enclose = i == 0 && s->length >= 2 && u_str_at(s, 0) == '|' && u_str_at(s, 1) == '|';
    if (i > 0) append(&t, ", ");
    if (enclose) append(&t, "(");
    append_str(&t, s);
    if (enclose) append(&t, ")");
  }
  append(&t, "|");
  return text_value(&t);
}

static u_value show_function(u_value v) {
  (void)v;
  return u_mk_lit("<function>");
}

static u_value show_procedure(u_value v) {
  (void)v;
  return u_mk_lit("<procedure>");
}

const struct u_type u_int_type = {"Int", compare_ints, u_equals_by_compare, show_int};
const struct u_type u_big_type = {"Int", compare_ints, u_equals_by_compare, show_int};
const struct u_type u_bool_type = {"Bool", compare_nums, u_equals_by_compare, show_bool};
const struct u_type u_char_type = {"Char", compare_nums, u_equals_by_compare, show_char};
const struct u_type u_str_type = {"Str", compare_strs, equal_strs, show_str};
const struct u_type u_tuple_type = {"*", compare_tuples, u_equals_by_compare, show_tuple};
const struct u_type u_array_type = {"array.Array", compare_arrays, u_equals_by_compare, show_array};
const struct u_type u_closure_type = {"->", compare_functions, compare_functions, show_function};
const struct u_type u_proc_type = {"proc", compare_procedures, compare_procedures, show_procedure};

// the error of the builtins on files, whose descriptor the runtime defines, since programs need not import os
const struct u_type t_os_46_OsError = {"os.OsError", u_con_compare, u_equals_by_compare, u_con_show};

void u_fail(const char *msg) {
  fflush(stdout);
  fprintf(stderr, "runtime error: %s\n", msg);
  exit(1);
}

// ends the program with the runtime error of a text of self/error.u, where the first %s stands for a and the second for b
static void fail_with(const char *format, u_value a, u_value b) {
  fflush(stdout);
  fputs("runtime error: ", stderr);
  for (const char *p = format; *p; p++) {
    if (p[0] == '%' && p[1] == 's') {
      u_fprint_value(stderr, a);
      a = b;
      p++;
    } else {
      fputc(*p, stderr);
    }
  }
  fputc('\n', stderr);
  exit(1);
}

void u_no_case(u_value v) {
  fail_with(u_text_no_case, v, NULL);
}

void u_assert_true(u_value v) {
  if (!u_truth(v)) fail_with(u_text_assertion_failed, NULL, NULL);
}

void u_assert_equal(u_value a, u_value b) {
  if (!u_equals(a, b)) fail_with(u_text_assertion_unequal, a, b);
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
  if (!handlers) fail_with(u_text_uncaught, v, NULL);
  handlers->value = v;
  handlers->type = type;
  longjmp(handlers->env, 1);
}

void u_reraise(struct u_handler *h) {
  u_raise(h->value, h->type);
}

// the directory of the modules of the library, which the build of a program can set with -DULANG_LIB=path
#ifndef ULANG_LIB
#define ULANG_LIB "prelude"
#endif

static void os_error(const char *message, u_value arg) {
  u_raise(u_mk_con(&n_os_46_OsError, 1, u_concat(u_mk_str(message), arg)), "os.OsError");
}

// the string of the UTF-8 of a name of the system, or else OsError with what it names
static u_value system_str(const char *bytes, const char *what) {
  u_value s = u_from_utf8(bytes, u_strlen(bytes));
  if (!s) os_error(what, u_mk_lit(" is not UTF-8"));
  return s;
}

// the contents of a file as UTF-8, or else NULL, where invalid UTF-8 raises OsError
static u_value read_file(const char *path, u_value name) {
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
  if (n != (size_t)st.st_size) return NULL;
  u_value t = decode(s, n, 1);
  if (!t) os_error("invalid UTF-8 in ", name);
  return t;
}

// the arguments of the program, after its name, which are decoded when they are asked for
static int argc = 0;
static char **argv = NULL;
static u_value args = NULL;

void u_args(int n, char **v) {
  argc = n;
  argv = v;
}

void p_os_args(u_value *result) {
  if (!args) {
    u_value list = &o_list_46_Nil;
    for (int i = argc - 1; i >= 1; i--) list = u_mk_con(&n_list_46_Cons, 2, system_str(argv[i], "an argument"), list);
    args = list;
  }
  *result = args;
}

void p_os_read(u_value path, u_value *result) {
  u_value t = read_file(u_utf8(path, NULL), path);
  if (!t) os_error("cannot read ", path);
  *result = t;
}

// the bytes of s as UTF-8 to f, and whether all were written
static int write_utf8(FILE *f, u_value s) {
  size_t n;
  const char *bytes = u_utf8(s, &n);
  return fwrite(bytes, 1, n, f) == n;
}

// creates the missing directories of path, as the interpreter does
void p_os_write(u_value path, u_value contents) {
  const char *file = u_utf8(path, NULL);
  char *dir = strcpy(text(u_strlen(file)), file);
  for (char *p = strchr(dir + 1, '/'); p; p = strchr(p + 1, '/')) {
    *p = 0;
    mkdir(dir, 0777);
    *p = '/';
  }
  FILE *f = fopen(file, "wb");
  if (!f) os_error("cannot write ", path);
  int ok = write_utf8(f, contents);
  if (fclose(f) || !ok) os_error("cannot write ", path);
}

static int compare_names(const void *a, const void *b) {
  return u_strcmp(*(char *const *)a, *(char *const *)b);
}

// the entries of dir without . and .., sorted by name, each with whether it is a directory, where links are not
// followed, such that a walk cannot loop, and the order of the bytes of UTF-8 is that of the code points
void p_os_list(u_value dir, u_value *result) {
  const char *name = u_utf8(dir, NULL);
  DIR *d = opendir(name);
  if (!d) os_error("cannot list ", dir);
  size_t n = 0, cap = 16;
  char **names = GC_MALLOC(cap * sizeof(char *));
  for (struct dirent *e = readdir(d); e; e = readdir(d)) {
    if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
    if (n == cap) names = GC_REALLOC(names, (cap *= 2) * sizeof(char *));
    names[n++] = strcpy(text(u_strlen(e->d_name)), e->d_name);
  }
  closedir(d);
  qsort(names, n, sizeof(char *), compare_names);
  u_value list = &o_list_46_Nil;
  for (size_t i = n; i-- > 0;) {
    char *path = text(u_strlen(name) + 1 + u_strlen(names[i]));
    sprintf(path, "%s/%s", name, names[i]);
    struct stat st;
    int is_dir = !lstat(path, &st) && S_ISDIR(st.st_mode);
    list = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, system_str(names[i], "a name in a directory"), u_mk_bool(is_dir)), list);
  }
  *result = list;
}

// path is a directory, also through a link
void p_os_is_dir(u_value path, u_value *result) {
  struct stat st;
  *result = u_mk_bool(!stat(u_utf8(path, NULL), &st) && S_ISDIR(st.st_mode));
}

void p_os_cwd(u_value *result) {
  char *dir = getcwd(NULL, 0);
  if (!dir) os_error("cannot tell the working directory", u_mk_lit(""));
  u_value s = u_from_utf8(dir, u_strlen(dir));
  free(dir);
  if (!s) os_error("the working directory", u_mk_lit(" is not UTF-8"));
  *result = s;
}

void p_os_chdir(u_value dir) {
  if (chdir(u_utf8(dir, NULL))) os_error("cannot change to ", dir);
}

void u_passed(const char *name) {
  printf("test %s passed\n", name);
}

void p_os_print(u_value text) {
  write_utf8(stdout, text);
  putchar('\n');
}

void p_os_print_error(u_value text) {
  fflush(stdout);
  write_utf8(stderr, text);
  fputc('\n', stderr);
}

void p_os_exit(u_value code) {
  fflush(stdout);
  exit(code->type == &u_int_type ? (int)code->num : 1);
}

void p_os_library(u_value name, u_value *result) {
  const char *base = u_utf8(name, NULL);
  char *path = text(u_strlen(ULANG_LIB) + u_strlen(base) + 3);
  sprintf(path, "%s/%s.u", ULANG_LIB, base);
  u_value t = read_file(path, name);
  if (!t) os_error("unknown module ", name);
  *result = t;
}
