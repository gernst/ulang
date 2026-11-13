# Every file a member, and one load per run

The plan of work for two changes that make runs over many files cost one load of each package: every `.u` file is a member of the package of its directory, so that there are no programs; and the driver loads one source for all its targets. It replaces the item of checking each package once per run, and decides part of Modules 3.

## 1. Why

A run of `test`, `check`, `lint`, `prove`, or `compile --tests` over many files groups them into units, a package with all its members or a program alone (`units_of` of `ulang/ulang.u`), and loads each unit on its own (`visit_units`, `check_file`): the parser assembles the source of the unit, its package, every package it imports, and the library, and the checker checks it (`check_source` of `ulang/check/check.u`). Within a unit nothing is parsed twice, but every unit parses and checks again the packages it imports, so the compiler is loaded once per case file of `ulang/test/errors/`, about 3 s each under load, and `check` of the project took 62 s and `compile --tests` 72 s, while the tests themselves take seconds.

Three choices compound there: a file without sections is a program, a module of no package, which loads alone; a package is one directory, so the tests sorted by feature into directories are many small units; and the driver loads each unit from scratch.

## 2. Every file is a member of the package of its directory

- A package is the directory, its members are all its `.u` files but `project.u`, and whether a file has sections no longer decides membership (`has_sections`, `load_package`, and `package_files` of `ulang/parse/parse.u`). A file without sections has all its commands implemented and exports nothing, as now; its private declarations, also its procedures without formals and its tests, stay private to it.
- There are no programs: `load_program` of `ulang/parse/parse.u` goes, and every file loads as a member, with its package. Which procedure runs is chosen on the command line, `run file.u:proc` and `compile file.u:proc`, by default `main` of that file, as now; several members may each have a private `main`.
- Any package may be imported, also one whose members export nothing, whose import then gives nothing; the parse error of an import of a package without members (`programs_only` of `ulang/test/errors/modules.u`) goes, and so do the tests of what programs see.
- A unit is a package: `units_of` of `ulang/ulang.u` groups the targets by directory only.
- What changes for the sources: `checks/`, `examples/`, and each directory of tests become packages, whose members must not export clashing names; `ulang/ulang.u` is the one member of the package `ulang`; a type error in one member fails its whole package, which all must pass anyway.
- `CLAUDE.md`, Modules and Tests, and `README.md` lose programs; the tests of modules that are programs, e.g. `ulang/test/modules/paths.u` and `ulang/test/modules/programs_only/`, are rewritten for members.
- The seed parses the sources of the tool, in which `ulang/ulang.u` is alone in its directory, so it needs no new seed.

## 3. Packages after one with errors are checked (Diagnostics 6)

`check_source` of `ulang/check/check.u` stops at the first package with errors, so with one source for a run, an error in one package would stop the checking of all after it. So first Diagnostics 6: a package after one with errors is checked against its signature, whose declarations with errors are error nodes, and a unit fails only by the errors of its own package and of those it imports.

## 4. One load per run

- The driver assembles one source for all the targets of a run, each package once, in load order, and checks it once; then it reports, runs the tests, proves, or lints per package from that one checked program, which already keeps every command with its module.
- Tests: a package keeps its tests if it is a target, not only if it is the package of the main file (`without_tests_of` of `ulang/parse/parse.u`); importers never see tests, so this is harmless.
- `Source` and `Prog` have one `main`, the module of the loaded file; for a run of many targets it is none, and the driver takes the tests and the entry by the modules of the targets, as `test_targets` and `compile_tests` of `ulang/ulang.u` do now by the files of a unit.
- Errors: each package fails by its own errors and those of the packages it imports, as now by unit (`is_runnable` of `ulang/ulang.u`).
- Measured before and after, under `nice -n 19`: `check` of the project, `make test`, and `compile --tests`.

## 5. Order

1. Every file a member (section 2), with the tests of modules rewritten; behaviour of runs over one package unchanged.
2. Diagnostics 6 (section 3).
3. One load per run (section 4).

Each step is a branch of its own; step 1 alone already makes `ulang/test/errors/` one load instead of seven.

## 6. Subdirectories

A package is exactly its directory, as in Go, and spans no subdirectories: `ulang/` is split into packages so that the imports check the layers of the phases, which a package spanning its subdirectories would merge, and with one load per run many small packages cost one load each. So `ulang/test` is the package of the tester, and each directory of tests below it a package of its own. Whether a package declares itself in a file inside it stays with Modules 3.
