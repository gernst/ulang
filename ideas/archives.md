# Packages compiled separately into static archives

The plan of Build 2 of `ideas/build.md`: each package is compiled once into a static archive, which programs link, and the library is the package `prelude`, built by the same mechanism, not a special one. What stays true afterwards goes into `CLAUDE.md`, `README.md`, or the items of `ideas/`.

## Why

- Every compiled program contains the C of all packages it loads, which `cc` parses for each program again: the C of an empty `main` is 106 KB, almost all library, and `cc -O2 -c` of it takes 70 ms; `tests/strings.u` is 123 KB and 175 ms, its check 50 ms, its compile to C 58 ms (measured 2026-11-11).
- Each test file of `ulang/` is compiled with the whole package of the compiler, about 600 KB of C, so `make testc` translates the same C once per file; this is presumably most of its 20 s, to be confirmed by step 0.
- The `static` linkage by `runtime_referenced` of `ulang/compile/lower.u` becomes unnecessary: the linker keeps only the sections that are reached.

## Why it works for ulang

- Every value is a `u_value`, and nothing is specialized by type, so one compiled `list.foldr` serves all callers.
- Checked programs carry only resolved names, `Name(Some(m), …)`, which `mangle` and `c_text` of `ulang/compile/lower.u` turn into C names that are the same in every translation unit.
- Orders, equality, and show are reached through the descriptors `t_…` of the types, `struct u_type` of `ulang/runtime/ulang.h`, never called by the users of a type.
- The front end checks every loaded package anyway, so each translation unit has the whole `Defs` and all signatures, and can declare what it references itself; no generated headers.

## Decisions

- **Static archives**, no shared libraries: these need position-independent code, rpaths or `LD_LIBRARY_PATH`, and versions of their own, for no gain in binaries that tests throw away.
- **One C file and one object per module**, a member of a package, collected in `out/lib/<package>.a`, where `<package>` is the id with `/` mangled as in C names; compiled with `-ffunction-sections -fdata-sections` and linked with `-Wl,--gc-sections`. Objects per module let `cc` run in parallel and let the archive drop whole members that nothing reaches.
- **Linkage**, three kinds instead of the two of `Linkage` of `ulang/compile/c.u`:
  - *defined once, extern*: functions, procedures, the caches of functions without parameters, descriptors `t_…` and names `n_…` of datatypes, of the module that declares them;
  - *defined once, hidden* (`__attribute__((visibility("hidden")))`): the private declarations of a module, which the tests of the member link; with static linking, hidden is extern within the binary; mangled names carry the module, so the privates of two members never clash;
  - *derived per unit, static*: what each translation unit builds from `Defs` for what it references, the wrappers `w_`, `wc_`, and `wp_` of functions, constructors, and procedures taken as values, the tables of fields of `u_select` and `u_update`, string literals, and the lifted lambdas, whose numbered names are then unique per unit only. Duplicates of wrappers and tables in several units are harmless and small.
- **Prototypes in each unit**: `extern` or hidden declarations of every function, procedure, descriptor, and name the unit references from other modules, from `Defs`, instead of headers; `prototypes` of `ulang/compile/compile.u` already writes those of a unit for itself.
- **The archive of `prelude` holds the runtime too**, `ulang/runtime/ulang.c` as one more object, and the formats `u_text_*` (`texts` of `ulang/compile/compile.u`), which every program now defines, since the runtime references them; the runtime's references to `t_list_46_List` and the like are resolved within the same archive.
- **Stamps**: each archive defines a symbol `u_stamp_<package>_<hash>` of the hash of its inputs: the sources of its members, the stamps of the packages it imports, the runtime, the flags, and the tool. Each unit that uses a package references the stamp it was compiled against, so a stale archive fails to link, not at run time.
- **The whole-program translation stays the default of `compile`**: the seed `bootstrap/ulang.c` remains one self-contained C file, and `make selfcompile` compares such files. The separate translation is a mode, used by `testc.sh` and later by `ulang.bin build`.

## Commands

- `ulang.sh compile --package <dir> <outdir>` checks the package of `<dir>` with its imports and writes `<outdir>/<module>.c` for each member, without tests, and the stamp. A Makefile rule or the caller runs `cc -c` on each and `ar rcs`.
- `ulang.sh compile --separate <file> <entry> <target.c>` writes only the modules of the program, or for a member of a package with the entry `test`, only the tests of that member, with `main`; linked by `cc target.c out/lib/<p>.a … out/lib/prelude.a -lgmp -lgc -Wl,--gc-sections`, with the archives in the order of loading reversed, users before what they use.
- `ulang.sh packages <file>` prints the ids of the packages it loads, in load order, for `testc.sh` to build their archives first.

## Code to change

- `ulang/compile.u`:
  - `compile_program` translates a selection of modules, `q.cmds` filtered by the module of each name, instead of all of `q.cmds`, with `Defs` still of the whole program; the whole-program mode selects all.
  - `wrappers` emits the wrappers of what the unit references, not of all of `Defs`, also in the whole-program mode, which then shrinks as well.
  - `descriptor_decls`, `data_decls`, and `field_table_decls` by module: descriptors where the datatype is declared, tables where they are used.
  - `Linkage` of `ulang/compile/c.u` gets `LkHidden`, and `linkage_of` decides by the module and by whether the declaration is exported, replacing `runtime_referenced`, which stays only for the whole-program mode.
  - `texts` only in the unit of the module `prelude`, and in the whole-program mode.
- `ulang/ulang.u`: the options of `compile` and the command `packages`.
- `ulang/runtime/ulang.h`: unchanged, since the descriptors it declares extern are extern by their module anyway.
- `Makefile`: `out/lib/prelude.a` from the members of `prelude/` and the runtime; `testc.sh` links it instead of `out/testc/ulang.o`.
- `testc.sh`: a first serial phase that builds the archives of every package that the files load, once each, then the parallel phase, which compiles each file with `--separate` and links the archives.
- `CLAUDE.md`, Build: the modes of `compile`, the archives in `out/lib/`, and the clash of a project with its own `prelude/` at the root, which now gives duplicate symbols at link time.

## Costs

- `cc` no longer inlines functions of other packages into their users, e.g. those of `option`; inlining at the level of ulang (Compiler 3) gets it back, since the front end sees the sources, and `-flto` meanwhile, at the cost of time. Compare the run times of the compiled compiler before and after.
- Two modes of `compile` to keep in step: `make testc` in the separate mode, and the whole-program mode exercised by `make selfcompile`, whose compiler must still pass `make test`.

## Steps

0. Time `make testc` per file, from the compile and `cc` of each, to confirm that the files of `ulang/` dominate; if they do not, the gain is about 70 ms per file, and the plan is still right but less urgent.
1. `LkHidden`, selection of modules, wrappers and tables per unit, in the whole-program mode first, which must give the same results of `make test` and `make testc`; compare the C before and after in a worktree, as `CLAUDE.md` asks.
2. `--package` and the archive of `prelude` with the runtime and `u_text_*`; `--separate` for programs that load only the library; `testc.sh` for those files.
3. Any package: `packages`, the serial phase of `testc.sh`, the tests of members against the archive of their own package, and stamps.
4. Measure `make testc` against step 0, and the run time of the compiled compiler, with and without `-flto`.
5. `ulang.bin build` (Build 1) builds and caches the archives itself, keyed by the stamps, which replaces the Makefile rule and the serial phase of `testc.sh`.

## Open

- Whether the tests of a member link the archive of their own package, which then holds the member's code without its tests, or compile the member into the test's unit and link the archive without that member, which needs an archive per member left out; the first is simpler and assumed above.
- Whether `ulang.sh compile --package` also runs `cc` and `ar`, which belongs to Build 1, or leaves them to the Makefile and `testc.sh` until then; assumed the latter.
