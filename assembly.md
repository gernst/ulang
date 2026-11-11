# Assembly

A plan, not implemented yet (TODO, Compiler 2): a second backend of the compiler that writes GNU assembly for x86-64 Linux instead of C, so that compiled programs need no C compiler, only `as` and a linker, while the runtime stays C. The C backend stays as the portable one and as the seed of the bootstrap.

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

The second to last row approximates a simple backend, which keeps every variable in memory as `-O0` does, next to the runtime at `-O2`: it is 1.35 times slower, since much of the time is spent in the runtime, in `compare`, allocation, and the collector. In exchange, `cc` no longer compiles programs: `make selfcompile`, which takes 36 seconds, spends about 25 of them in `cc -O2`, which would become about a second of `as` and linking, and `make testc` would compile each file in a fraction of the time.

## Approach

**`ulang/asm.u` translates a `CFile` of `ulang/compile/c.u` to assembly**, so that `compile.u` stays as it is, and only the last step differs: `show_file(f)` for C, and `asm_file(f)` for assembly. The part of C that `compile.u` writes is small, 11 kinds of expressions, 7 of statements, and 4 of declarations, which keeps the backend small. A backend from the syntax of ulang directly would duplicate the lowering of `compile.u` (matches, comprehensions, closures, lifted lambdas, the saved variables of `try`) and is not needed for a first version.

- **Target**: x86-64 with the System V calling convention, as on Linux, in the syntax of GNU `as`.
- **Locals**: every `CsDecl` of a function gets a slot of its own in its frame, also those of nested blocks, and all variables live in memory, which is slow but simple, and makes `setjmp` harmless, since no value stays in a register across it. The scheme of `try`, which saves the variables that the body assigns before the `setjmp` and restores them in the handler, carries over unchanged.
- **Expressions** are computed into `%rax`, where intermediate results are pushed. Arguments go to `%rdi`, `%rsi`, `%rdx`, `%rcx`, `%r8`, and `%r9`, and further ones to the stack, which is aligned to 16 bytes at each call, and `%al` is 0 for the variadic functions of the runtime, such as `u_mk_con`, `u_mk_tuple`, `u_mk_closure`, and `u_call_proc`, which take many arguments.
- **Control flow**: `&&`, `||`, `!`, `?:`, `if`, and `while` are comparisons and jumps to numbered labels, and `return` jumps to the epilogue of the function.
- **Static data**: the literals of strings (`U_LIT`), the type descriptors `t_` (`struct u_type`), the constructor names `n_` (`struct u_name`), the units `o_`, and the `static` locals of functions, which are the caches of functions without parameters and literals, go to `.data` and `.rodata` with labels of their own, where function pointers in descriptors and closures are relocations.
- **Exceptions** call the runtime as now: `u_push`, `setjmp` of the C library on the `jmp_buf` of the handler, `u_pop`, `u_raise`, and `u_reraise`.
- **The runtime** stays C, compiled once at `-O2` to `ulang/runtime/ulang.o` (as `make testc` does with `out/testc/ulang.o`), and a program is linked by `cc program.s ulang.o -lgmp -lgc`, where `cc` only assembles and links.

## Difficulties

1. **The layout of the structures of the runtime**: the assembly needs the offsets of the fields of `u_object` (`type`, `num`, `text`, `length`, `name`, `size`, `items`), `u_name`, `u_type`, and `u_handler`, and their sizes, which only the C compiler knows. `asm.u` repeats them, and the runtime states them by `_Static_assert(offsetof(struct u_object, items) == 24, ...)`, so that a change of `ulang.h` fails when the runtime is compiled instead of breaking programs silently.
2. **The inline functions of `ulang.h`**: `u_field`, `u_is_con`, `u_truth`, and `u_order` are the most frequent operations of generated code, `u_field` 4,273 and `u_is_con` 2,037 times in the C of the compiler, along with `U_LIT` (920), `U_UNIT`, and `U_COMPUTING`. The first version calls them as functions, which the runtime then exports as well, and a later one writes their few instructions in place, loads and comparisons.
3. **Portability**: the backend is tied to x86-64 and the System V convention, so ARM64, e.g. on macOS, would need a second one with its own instructions and convention, and other systems differ in details such as the underscore of symbols and the sections. The C backend therefore stays, for other platforms and as the seed of the bootstrap.
4. **Debugging**: errors of the backend show as crashes without the help of a debugger. Each statement is preceded by a comment with its C, `asm.u` has unit tests of small functions, and `make testc` compares every test in both backends.
5. **Speed**: the simple backend costs about 1.35 times on the compiler. Getting to `-O2` would take register allocation for locals and temporaries, the hard part, which is worth it only after measuring, after the simpler improvements of step 5.

## Steps

0. **Compiled tests**, done: `make testc`, which the backend needs as its safety net, since every test must have the same outcome in both backends.
1. **The runtime**: compiled once to `ulang.o`, the `_Static_assert`s of the offsets, and the inline functions of `ulang.h` exported as functions too.
2. **`ulang/asm.u`**: the translation of a `CFile` as above, with unit tests that assemble and run small functions.
3. **`ulang.bin compile <file> <entry> <target.s>`**, which chooses the backend by the extension of the target, and `make testc` with both backends.
4. **Bootstrap**: `make selfcompile` through assembly, whose compiler must compile itself to the same `.s` file as well as to the same C, where `bootstrap/ulang.c` stays the versioned seed.
5. **Simple improvements**, each measured on the self-compile: the instructions of `u_field`, `u_is_con`, and `u_truth` in place, results that stay in `%rax` instead of going through their slot, and arguments that are variables or constants loaded directly into the registers of arguments.
6. **Later, each a project of its own**: register allocation, ARM64, and unboxed small integers, which `TODO.md` rejected for the C backend, and which a backend of its own makes easier.
