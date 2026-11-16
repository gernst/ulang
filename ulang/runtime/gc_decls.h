// the part of the Boehm collector that the runtime uses: gc.h where it is installed, else the declarations below of the
// ABI of libgc.so.1 on 64-bit Unix, without threads and debugging, as gc.h has them then, so that a build needs only the
// shared library; ULANG_OWN_DECLS takes the declarations below also where gc.h is installed, to test them

#ifndef ULANG_GC_DECLS_H
#define ULANG_GC_DECLS_H

#if defined(__has_include) && !defined(ULANG_OWN_DECLS)
#if __has_include(<gc.h>)
#define ULANG_HAS_GC_H
#endif
#endif

#ifdef ULANG_HAS_GC_H
#include <gc.h>
#else
#include <stddef.h>

typedef unsigned long GC_word;
typedef void (*GC_finalization_proc)(void *, void *);

void GC_init(void);
void *GC_malloc(size_t);
void *GC_malloc_atomic(size_t);
void *GC_malloc_many(size_t);
void *GC_realloc(void *, size_t);
size_t GC_size(const void *);
int GC_expand_hp(size_t);
void GC_set_free_space_divisor(GC_word);
void GC_register_finalizer(void *, GC_finalization_proc, void *, GC_finalization_proc *, void **);

#define GC_INIT() GC_init()
#define GC_MALLOC(n) GC_malloc(n)
#define GC_MALLOC_ATOMIC(n) GC_malloc_atomic(n)
#define GC_REALLOC(p, n) GC_realloc(p, n)
#define GC_REGISTER_FINALIZER(p, f, d, of, od) GC_register_finalizer(p, f, d, of, od)
#endif

#endif
