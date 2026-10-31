# ulang

A minimal programming language with pure functional expressions, imperative statements, datatypes, and Hindley-Milner type inference, written in Scala 3.

## Build

Mill with `build.mill.yaml`; the language lives in the module `ulang`. Do not use sbt.

- `./mill ulang.compile`
- `./mill ulang.run file.u ...` checks and runs the files, printing the values of `eval`s and the variables of `run`s
- `./mill ulang.test` runs every `tests/**/*.u` and `examples/**/*.u` (utest, `FileTest`)
- `make` builds `ulang.jar`, which `./ulang.sh file.u ...` runs (the module directory `ulang/` prevents a script `./ulang`)
- `./ulang.sh --tests file.u ...` runs each test on its own and prints `test n passed` or `test n failed: ...`; `./ulang.sh --only n file.u` runs only the `n`-th `run` or `test` of the file
- `make install` packages the VS Code extension in `vscode/` as `ulang.vsix` and installs it into VSCodium (`codium`): syntax highlighting, a run button, `▶ Run` links above `run` and `test`, errors in the Problems view, and the tests in the Testing view, all by running `ulang.jar`

## Tests

Tests are end-to-end only: programs in `tests/` with `test ... end` blocks of `assert` statements.
Every program is checked after `ulang/resources/prelude.u`, which declares the operators and imports the library modules `nat`, `option`, and `list` (also in `ulang/resources/`).
`import name` inlines the declarations of the module `name.u` from the importing file's directory, or else from the library, once, while parsing, so its notations apply after the import; its `eval`s, `run`s, and `test`s are not imported.
A file that is expected to fail states the error in its first line, e.g. `// type error: unknown variable x`, see `tests/errors/`.

## Style

- Omit comments that are self-evident from the code, such as doc comments that restate the name of a declaration.
