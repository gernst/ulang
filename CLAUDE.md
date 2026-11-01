# ulang

A minimal programming language with pure functional expressions, imperative statements, datatypes, and Hindley-Milner type inference, written in Scala 3.

## Build

Mill with `build.mill.yaml`; the language lives in the module `ulang`. Do not use sbt.

- `./mill ulang.compile`
- `./mill ulang.run file.u ...` checks and runs the files, printing the values of `eval`s and the variables of `run`s
- `./mill ulang.test` runs every `tests/**/*.u`, `examples/**/*.u`, and `self/**/*.u` (utest, `FileTest`)
- `make` builds `ulang.jar`, which `./ulang.sh file.u ...` runs (the module directory `ulang/` prevents a script `./ulang`)
- `./ulang.sh --tests file.u ...` runs each test on its own and prints `test name passed` or `test name failed: ...`; `./ulang.sh --only name file.u` runs only the `run` or `test` of that name
- `self/compile.u` compiles ulang programs to C, where its test writes the translations of some tests and examples to `self/out/`, which `cc -DULANG_LIB='"'$PWD/ulang/resources'"' -I self/runtime self/out/NAME.c self/runtime/ulang.c -lgmp -lgc` compiles with the runtime, using GMP and the Boehm collector, where `ULANG_LIB` is the directory of the library for `os_library`
- `make install` packages the VS Code extension in `vscode/` as `ulang.vsix` and installs it into VSCodium (`codium`): syntax highlighting, a run button, `▶ Run` links above `run` and `test`, errors in the Problems view, and the tests in the Testing view, all by running `ulang.jar`

## Tests

Tests are end-to-end only: programs in `tests/` with named tests `test name := ...` of `assert` statements, where runs `run name := ...` are named as well, and the names of runs and tests are distinct.
Every program is checked after `ulang/resources/prelude.u`, which declares the operators and imports the library modules `nat`, `option`, and `list` (also in `ulang/resources/`).
The library module `os` declares `OsError`, and with it come the builtin procedures `os_args`, `os_read`, `os_write`, and `os_library`, where the latter raise `OsError` when they fail, and `os_args` gives the arguments after `--`, e.g. `./ulang.sh self/ulang.u -- source.u target.c`.
`import name` inlines the declarations of the module `name.u` from the importing file's directory, or else from the library, once, while parsing, so its notations apply after the import; its `eval`s, `run`s, and `test`s are not imported.
A file that is expected to fail states the error in its first line, e.g. `// type error: unknown variable x`, see `tests/errors/`.

## Style

- Omit comments that are self-evident from the code, such as doc comments that restate the name of a declaration.
