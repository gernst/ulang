# ulang

A minimal programming language with pure functional expressions, imperative statements, datatypes, and Hindley-Milner type inference, written in Scala 3.

## Build

Mill with `build.mill.yaml`; the language lives in the module `ulang`. Do not use sbt.

- `./mill ulang.compile`
- `./mill ulang.run file.u ...` checks and runs the files, printing the values of `eval`s and the variables of `run`s
- `./mill ulang.test` runs every `tests/**/*.u` (utest, `FileTest`)
- `make` builds `ulang.jar`, which `./ulang.sh file.u ...` runs (the module directory `ulang/` prevents a script `./ulang`)
- `make install` packages the VS Code extension in `vscode/` (syntax highlighting) as `ulang.vsix` and installs it into VSCodium (`codium`)

## Structure

- `ulang.source`: the surface syntax as written, with positions, and its context-free `Parser`
- `ulang.Resolve`: turns the surface syntax into the core syntax, resolving operators by their notations, sugar, and imports
- `ulang.core`: the core syntax, `Check` (type inference and elaboration), `Run` (interpreter), and `Printer` (values)
- `ulang.Main`: loads the prelude and runs files

## Tests

Tests are end-to-end only: programs in `tests/` with `test ... end` blocks of `assert` statements.
Every program is checked after `ulang/resources/prelude.u`, which declares the operators and imports the library modules `nat`, `option`, and `list` (also in `ulang/resources/`).
`import name` inlines the module `name.u` from the importing file's directory, or else from the library, once, while parsing, so its notations apply after the import.
A file that is expected to fail states the error in its first line, e.g. `// type error: unknown variable x`, see `tests/errors/`.

## Style

- Omit comments that are self-evident from the code, such as doc comments that restate the name of a declaration.
