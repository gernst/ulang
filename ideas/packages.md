# One load per run

Every `.u` file is a member of the package of its directory, and packages after one with errors are checked against its signature (formerly Diagnostics 6). A run over many files caches what it has parsed and checked: `parse_file_loaded` of `ulang/parse/parse.u` takes the packages loaded before, `check_cached` of `ulang/check/check.u` those checked before without errors, and `load_file` of `ulang/ulang.u` threads both through the units of `visit_units` and `compile_tests`, so `check` of the project took 9.9 s instead of 22.8 s. What is left is a simplification, and a speedup only if a profile shows one.

## 1. Why

Each unit, a package of targets, still parses and checks its own package, joins the signatures of the cached ones, and loads the tester; the cache, threaded through `load_file` as `Cache` of `ulang/ulang.u`, and the bookkeeping of `compile_tests`, which takes each module from the unit that it is a member of, exist only because the driver loads per unit.

## 2. One source for all targets

- The driver assembles one source for all the targets of a run, each package once, in load order, and checks it once; then it reports, runs the tests, proves, or lints per package from that one checked program, which already keeps every command with its module, and `Cache` goes.
- Tests: a package keeps its tests if it is a target, not only if it is the package of the main file (`without_tests_of` of `ulang/parse/parse.u`); importers never see tests, so this is harmless.
- `Source` and `Prog` have one `main`, the module of the loaded file; for a run of many targets it is none, and the driver takes the tests and the entry by the modules of the targets, as `check_unit` and `compile_tests` of `ulang/ulang.u` do now by the files of a unit.
- Errors: a unit fails only by the errors of its own package and of those it imports, so with one source the diagnostics are filtered per package by the packages it sees (`seen_modules` of `ulang/syntax/syntax.u`), where per unit the source holds no others.
- `units` of `ulang/check/check.u` and `module_package` of `ulang/parse/parse.u` take the package of a module by its name, `module_dir`, as `check_source` does, instead of finding it in `Source.packages`, without their cases of a module of no package, which only the fixtures of tests make; their modules then take names with directories, one per package: those of `ulang/syntax/fixtures.u`; `bags` of `ulang/check/check.u`; `parse_text_recovering` of `ulang/parse/parse.u`; and the source that the tests of `ulang/check/hints.u` build.
- Before starting: a gprof profile of the compiled `check` of the project, to tell whether the time left is in the units' own packages, which one source does not save, or in the joins of signatures and the loads of the tester, which it does.

## 3. Subdirectories

A package is exactly its directory, as in Go, and spans no subdirectories: `ulang/` is split into packages so that the imports check the layers of the phases, which a package spanning its subdirectories would merge, and with one load per run many small packages cost one load each. So `ulang/test` is the package of the tester, and each directory of tests below it a package of its own. Whether a package declares itself in a file inside it stays with Modules 3.
