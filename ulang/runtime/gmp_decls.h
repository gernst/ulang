// the part of GMP that the runtime uses: gmp.h where it is installed, else the declarations below of the ABI of
// libgmp.so.10 on 64-bit Unix, where a limb is an unsigned long, so that a build needs only the shared library;
// ULANG_OWN_DECLS takes the declarations below also where gmp.h is installed, to test them

#ifndef ULANG_GMP_DECLS_H
#define ULANG_GMP_DECLS_H

#if defined(__has_include) && !defined(ULANG_OWN_DECLS)
#if __has_include(<gmp.h>)
#define ULANG_HAS_GMP_H
#endif
#endif

#ifdef ULANG_HAS_GMP_H
// stdio.h before gmp.h, which declares its functions on files only after it
#include <stdio.h>
#include <gmp.h>
#else
#include <stddef.h>

typedef unsigned long mp_limb_t;
typedef struct {
  int _mp_alloc;
  int _mp_size;
  mp_limb_t *_mp_d;
} __mpz_struct;
typedef __mpz_struct mpz_t[1];
typedef __mpz_struct *mpz_ptr;
typedef const __mpz_struct *mpz_srcptr;

// the names of the functions are macros of the symbols of the library, as in gmp.h
void __gmpz_init(mpz_ptr);
void __gmpz_set(mpz_ptr, mpz_srcptr);
void __gmpz_set_si(mpz_ptr, long);
int __gmpz_set_str(mpz_ptr, const char *, int);
int __gmpz_fits_slong_p(mpz_srcptr);
long __gmpz_get_si(mpz_srcptr);
char *__gmpz_get_str(char *, int, mpz_srcptr);
int __gmpz_cmp(mpz_srcptr, mpz_srcptr);
int __gmpz_cmp_si(mpz_srcptr, long);
void __gmpz_add(mpz_ptr, mpz_srcptr, mpz_srcptr);
void __gmpz_sub(mpz_ptr, mpz_srcptr, mpz_srcptr);
void __gmpz_mul(mpz_ptr, mpz_srcptr, mpz_srcptr);
void __gmpz_tdiv_q(mpz_ptr, mpz_srcptr, mpz_srcptr);
void __gmpz_tdiv_r(mpz_ptr, mpz_srcptr, mpz_srcptr);
void __gmp_set_memory_functions(void *(*)(size_t), void *(*)(void *, size_t, size_t), void (*)(void *, size_t));

#define mpz_init __gmpz_init
#define mpz_set __gmpz_set
#define mpz_set_si __gmpz_set_si
#define mpz_set_str __gmpz_set_str
#define mpz_fits_slong_p __gmpz_fits_slong_p
#define mpz_get_si __gmpz_get_si
#define mpz_get_str __gmpz_get_str
#define mpz_cmp __gmpz_cmp
#define mpz_cmp_si __gmpz_cmp_si
#define mpz_add __gmpz_add
#define mpz_sub __gmpz_sub
#define mpz_mul __gmpz_mul
#define mpz_tdiv_q __gmpz_tdiv_q
#define mpz_tdiv_r __gmpz_tdiv_r
#define mp_set_memory_functions __gmp_set_memory_functions
#define mpz_sgn(z) ((z)->_mp_size < 0 ? -1 : (z)->_mp_size > 0)
#endif

#endif
