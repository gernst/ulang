# The tester

A plan: the tests that the tools generate, and the running of tests, move out of the checker, the driver, and the library into the package `ulang/test`, the files at the top of `ulang/test/`, whose subdirectories keep the tests of the language. When this work is done, what holds goes into `CLAUDE.md`, what stays open into the items of `ideas/testing.md`, and this file is deleted.

## Why

- Whether a lemma holds is for `prove` alone: enumerating values can falsify a lemma, never establish it. So the tests `l.lemma` are no check of validity, which the checker would need, but a step before the proof: a counterexample tells an agent more than an open goal, a falsified lemma needs no proof, and a lemma whose proof stays open is at least tested (Verification 2).
- So only `test` and `compile --tests` need them, while before, `check_package` of `ulang/check/check.u` generated them on every check without errors, and every load of every command parsed and checked the enumeration as a member of the library that no module but the generated tests saw, hidden by a special case of `implicit_imports` of `ulang/syntax/syntax.u`.
- The tests with formals of Testing 1, property-based tests, are the general form: the test of a lemma `requires a ensures b` is the test with the formals of the lemma whose body is `if a then assert b`, with a count for `Vacuous`. One translation serves both, and the enumeration is the library of the tester, not of the language.

## Layout (step 1, done)

- `ulang/test/generate.u`: the translation of lemmas, later of tests with formals, into tests over enumerated values (`lemma_tests`), which `check_all` of `ulang/check/check.u` takes as a hook, a function from what a package declares (`Checked` of `ulang/check/check.u`) to generated commands, which it checks after the definitions of the package; the driver passes it for `test` and `compile --tests` and `no_tests` else, and `ulang/compile` and `ulang/check` do not import `ulang/test`.
- `ulang/test/runner.u`: the running of the tests of a module, each on its own, and then the proofs of its lemmas (`test_module` and `prove_module`), while the walk of targets and the lines per file stay in the driver, as they serve `check`, `lint`, and `prove` too. `ulang/test` comes after `ulang/run` and `ulang/prove`, which it imports, as `ulang/test/cases` does.
- `ulang/test/enumerate/enumerate.u`: the enumeration. In this project it is a package of its own, whose examples `make test` runs; into a program under test the tester loads it as the package `(test)` of one member `(test)/enumerate`, a name that no path can give, which no module sees and the generated tests name by qualified names, read by `os_library` from `ULANG_LIB/../test/enumerate/enumerate.u`, so that no second path is compiled in (`load_tester` of `ulang/test/runner.u`).

## Steps

1. The layout above, done.
2. `make selfcompile`, so that the seed no longer loads the enumeration as a member of the library, and its file in `ulang/prelude/` goes: done.
3. A lemma whose test is falsified is reported as `lemma l falsified` and not proven: done (`test_module` and `prove_module` of `ulang/test/runner.u`).
4. Tests with formals (Testing 1) by the same translation, without `TestParams`.
5. The output of Testing 2 in both backends.
