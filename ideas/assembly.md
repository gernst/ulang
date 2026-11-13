# Assembly

In progress (Compiler 2 of `ideas/compiler.md`), steps 0 to 4 done and 5 by hand: a second backend of the compiler that writes GNU assembly for x86-64 Linux instead of C, so that compiled programs need no C compiler, only `as` and a linker, while the runtime stays C. The C backend stays as the portable one and as the seed of the bootstrap.

## Why

Measured on the compiler compiling itself, on the sources of commit 22b0daf:

| | time |
|---|---|
| `cc -O2` of the C of the compiler (32,000 lines) | 11.2 s |
| `cc -O0` of the same | 2.2 s |
| `as` of its assembly (149,000 lines, as `cc -O2 -S` writes it) | 0.2 s |
| self-compile, generated code and runtime at `-O2` | 0.45 s |
| self-compile, generated code at `-O0`, runtime at `-O2` | 0.61 s |
| self-compile, both at `-O0` | 0.85 s |

The second to last row approximates a simple backend, which keeps every variable in memory as `-O0` does, next to the runtime at `-O2`: it is 1.35 times slower, since much of the time is spent in the runtime, in `compare`, allocation, and the collector. In exchange, `cc` no longer compiles programs: `make selfcompile` spends most of its time in `cc -O2`, which would become about a second of `as` and linking.

## Decision: from the checked program, not from the C

The backend translates the checked program, `Prog` of `ulang/syntax/syntax.u`, directly to instructions, not the `CFile` that `ulang/compile/compile.u` makes:

- The C of `compile.u` is shaped by C: matches set a flag `d` that every later case tests, a `match` expression assigns a result variable that starts as `NULL`, `&&` chains of pattern tests, temporaries declared in nested blocks, and the macros and inline functions of `ulang/runtime/ulang.h` (`U_LIT`, `U_UNIT`, `u_mk_lit`, `u_field`, `u_is_con`, `u_truth`, `u_select`), whose meaning only the C compiler knows. Translated to assembly, each becomes a detour; from the source, a case is tests that jump to the next case, a match a sequence of them, and the inline functions are the few loads and comparisons they stand for.
- What is not shaped by C is shared, not duplicated: the names (`mangle`, `fun_name`, `proc_name`, `type_key`, …), the tables of the runtime (`builtin_functions`, `runtime_referenced`, …), and the analyses of the source (`free_vars`, `free_in_stmt`, `changed`, `fusion_of`) move from `compile.u` into a member `ulang/compile/lower.u` of the same package, which both backends use. What remains duplicated is the walk over expressions and statements, which is the backend itself.
- A later optimizing backend (registers, unboxed integers) needs the source anyway, not C.

## Design

- **Files**: `ulang/compile/asm.u`, the datatypes of x86-64 assembly and their printer in the syntax of GNU `as` (AT&T), with a test of the printer, `format_asm_lines`; `ulang/compile/x86.u`, the translation of a `Prog` to an `AsmFile`; `ulang/compile/lower.u`, the shared part above. All members of `ulang/compile`.
- **The datatypes**: `Reg`, `Operand`, `Cond`, `Instr`, `Directive`, and `Line` (labels and comments) of `ulang/compile/asm.u`, with `AsmFile`; the source documents the instructions and directives. Code is built as these values and printed only at the end, `format_asm(f)`.
- **Target**: x86-64 with the System V calling convention, position-independent as the default PIE of the linker wants: symbols addressed relative to `%rip`, functions of the runtime and the C library called by `@PLT`, and the addresses inside data (descriptors, closures in tables) as `.quad` relocations, in writable `.data`.
- **Frames**: every variable, out and ref copy, captured value, and temporary has a slot of its own at a fixed offset below `%rbp`, allocated by a counter per function, and the size of the frame, rounded to 16, is written into the prologue after the body is translated. All values live in memory, so `setjmp` is harmless, since no value stays in a register across it, and `%rsp` is aligned to 16 at every call.
- **Expressions** are computed into `%rax`. The arguments of a call are computed one after another into temporaries, then loaded into `%rdi`, `%rsi`, `%rdx`, `%rcx`, `%r8`, and `%r9`, and further ones pushed in reverse, with a padding word if their number is odd; `%al` is 0 for the variadic functions of the runtime (`u_mk_con`, `u_mk_tuple`, `u_mk_closure`, `u_mk_proc`, `u_apply`, `u_call_proc`, `u_mk_array`).
- **The inline functions of `ulang.h`** are written in place: `u_truth` is `cmpq $0, 8(%rax)`, `u_is_con` a comparison of `8(%rax)` with the address of the descriptor, `u_field` two loads, `u_select` the load of the index of the constructor from its `u_name` and of the position from the table, with a call of `u_missing_field` if it is negative; `u_mk_lit` is `u_mk_units` with the number of units known; `U_COMPUTING` is -1.
- **Control flow**: `if`, `while`, `and`, `or`, `if … then … else`, and the cases of `match` and `catch` are tests and jumps to labels numbered per file. A case tests the pattern against the scrutinee in its slot and jumps to the next case at the first test that fails, then binds the variables, tests the guard, and runs the body, which jumps to the end of the match; after the last case comes the call of `u_no_case`, or of `u_reraise` for handlers.
- **Exceptions** call the runtime as the C does: `u_push`, `_setjmp` of the C library on the `jmp_buf` at the start of the handler, `u_pop`, `u_raise`, `u_reraise`, `u_has_type`, and `u_caught`; the variables that the body of a `try` assigns are saved before and restored in the handler, since handlers see the variables as before the `try`, which is semantics, not an artefact of C.
- **Static data**: the descriptors `t_` (`struct u_type`), `n_` (`struct u_name`), the units `o_`, the tables `x_` and texts `lf_` of fields, the literals of strings as `struct u_object` with their units of 1, 2, or 4 bytes ending with a 0 in `.rodata`, the caches of functions without parameters, the formats `u_text_*`, and the tables `u_files` and `u_tests` of `EntryTests`; the extern ones, those of `runtime_referenced` and `u_text_*` and `main`, are `.globl`. The layouts: `u_object` 32 bytes, `type` 0, `num`, `units`, and `name` 8, `length` and `size` 16, `width` and `items` 24; `u_name` and `u_type` 32 bytes, each field 8, `index` an `int` at 16; `u_handler` 232 bytes, `env` at 0; `u_test` 24 bytes. `x86.u` states them once, and the runtime checks them by `_Static_assert`, so that a change of `ulang.h` fails when the runtime is compiled instead of breaking programs silently.
- **The runtime** stays C, compiled once at `-O2` to `out/asm/ulang.o`, and a program is linked by `cc program.s out/asm/ulang.o -lgmp -lgc`, where `cc` only assembles and links.
- **Driver**: `compile <file> -o <target>` writes assembly if the target ends in `.s` and C otherwise, also with `--tests`.

## Difficulties

1. **Debugging**: errors of the backend show as crashes. Planned: each statement preceded by a comment naming it, which `ulang/compile/x86.u` does not emit yet. Every test runs in both backends, `make testc` and `make testasm`, with the same outcome. If that is not enough, the fallback is an interpreter of `AsmFile` in ulang, with the runtime calls it needs, which reports a fault at its instruction instead of crashing.
2. **Portability**: tied to x86-64 and System V; ARM64, e.g. macOS, would need a second instruction set and convention, so the C backend stays.
3. **Speed**: the simple backend is expected about 1.35 times slower than `-O2` on the compiler; registers for locals and temporaries come only after measuring.

## Results

On the sources of 5496b96, when the branch `asm` was merged, by `out/ulang.bin` (C at `-O2`) and by the compiler built through assembly:

| | time |
|---|---|
| `compile ulang/ulang.u` to C, by the C build | 3.1 s |
| the same, by the assembly build | 3.4 s |
| `compile ulang/ulang.u` to `.s` (339,000 lines), by the assembly build | 4.7 s |
| `cc` of that `.s` with `out/asm/ulang.o`, assembling and linking | 0.7 s |
| `cc -O2` of the C of the compiler, for comparison | about 11 s |
| `cc` of the `.s` of `compile --tests` (506,000 lines) | 0.9 s |
| `make testasm`, running the suite | 16 s, against 7 s of `make testc` at `-O1` |

The simple backend is about 1.1 times slower than `-O2` on the compiler, less than estimated, but about twice as slow on the suite, whose tests spend more in generated code than in the runtime. The assembly build compiles itself to the same `.s` and the same C as the C build.

## Steps

0. **Compiled tests**, done: `make testc`, the safety net, since every test must have the same outcome in both backends.
1. Done. **`lower.u`**: the shared part moved out of `compile.u`, which then writes the same C as before, byte for byte.
2. Done. **`asm.u`**: the datatypes and the printer, with a test.
3. Done. **`x86.u`** with the runtime object and the `_Static_assert`s: data, functions, procedures, statements, expressions, closures, wrappers, caches, `try`, then `compile … -o x.s` in the driver; first without the fusion of comprehensions (`fusion_of`), which only saves the calls of folds. Grown on small programs of `ulang/test/`, each compared with its C build.
4. Done. **`make testasm`**: the suite of `compile --tests` through assembly, all tests passing as with `make testc`.
5. Done by hand, not yet as a target of the Makefile. **Self-compile through assembly**: the compiler built from its `.s` compiles itself to the same C and the same `.s`; `make` keeps building the tool from C until this is reliable, then may switch; `bootstrap/ulang.c` stays the versioned seed.
6. **Measured improvements**: the fusion of comprehensions as in C, results that stay in `%rax` instead of a slot, arguments that are variables or constants loaded directly into their registers.
7. **An encoder of its own**: the instructions of `asm.u` encoded to machine code and written as an ELF relocatable object, so that neither `as` nor a C compiler is needed, only the linker. It needs bytes in files first, which `os_write` cannot write, since it writes strings as UTF-8 (Library 1 of `ideas/library.md`).
8. **Later, each a project of its own**: register allocation, ARM64, unboxed small integers.
