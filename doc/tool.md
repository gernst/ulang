# The tool

`ulang.sh` is the one binary of ulang, with the commands `run`, `check`, `hints`, `format`, `compile`, `refactor`, and `help`, and the Makefile builds it and runs the tests. This file describes each command with its options and replies; `doc/language.md` is the language and `doc/implementation.md` the implementation.

## Building

ulang requires a C compiler, GMP, and the Boehm garbage collector, the packages `libgmp-dev` and `libgc-dev` on Debian and Ubuntu, `gmp` and `gc` on Arch. Their headers are optional: without the packages for development, their shared libraries `libgmp.so.10` and `libgc.so.1` suffice, since the runtime takes `gmp.h` and `gc.h` where they are installed, else its own declarations of what it uses of their shared libraries, `ulang/runtime/gmp_decls.h` and `ulang/runtime/gc_decls.h`, which `-DULANG_OWN_DECLS` takes also where the headers are installed, to test them, and `LIBS` of the Makefile links `-lgmp -lgc`, else the files `libgmp.so.10` and `libgc.so.1`.

- `make` builds the tool `out/ulang.bin`, the compiler of the current sources `ulang/*.u` and `ulang/*/*.u`, with the library in `ulang/prelude/`: the seed `bootstrap/ulang.bin`, built from `bootstrap/ulang.c`, the C of the last compiler that compiled itself to the same C, translates them to `out/ulang.c`. Each binary is linked with the runtime, GMP, and the Boehm collector, and rebuilt when its inputs change: about 15 seconds from nothing, 7 after an edit of the sources; `rm -rf out` loses nothing that `make` cannot rebuild. `ULANG_LIB` is the directory `ulang/prelude/` of the library, for imports and `os_library`.
- The interpreter and the compiler recurse as deeply as the programs they run, so each binary is run by a copy of `bootstrap/ulang.sh` of its name next to it, `out/ulang.sh` for `out/ulang.bin`, which `make` makes and calls, and copies with the binary to the root, `ulang.sh` and `ulang.bin`, which are not versioned, and which sets `ulimit -s unlimited`, else the hard limit, before it runs the binary; so the commands below are `out/ulang.sh run …` and the like, never `out/ulang.bin` directly. Compiled programs need the same limit, so the binaries of the Makefile have copies of the script, too.
- Every command runs the built `out/ulang.bin`, but the tests that `check --test` runs are interpreted, with the current sources of what they load, also of the compiler: after an edit of `ulang/`, its tests see the edit at once, while `run`, `format`, `hints`, and the checking by `check` itself see it only after `make`.
- `make selfcompile` compiles the sources with `out/ulang.bin` as it is, built by the seed only if it is missing, so that a binary built from the commit that teaches a syntax compiles the sources that use it, which the seed cannot parse; `make bootstrap` runs the compiler on the sources in the interpreter of the seed (`bootstrap/ulang.sh run ulang/ulang.u -- compile …`), far too slow for use, only a last resort for sources that the parser and the checker of the seed accept but its code generator compiles wrong, since its interpreter parses and checks as the seed does. Both replace the seed, `bootstrap/ulang.c` and `bootstrap/ulang.bin`, only if the new compiler compiles itself to the same C, keep the previous pair in `bootstrap/<time stamp>/`, and then rebuild `out/ulang.bin`. Their stages are in a directory `out/stage-*` of each run, removed if it succeeds. The C of the seed is versioned, the binaries are not, and of the backups only those worth keeping are committed. `make selfcompile` takes about 15 seconds, `make bootstrap` about 4 minutes.
  - A new syntax reaches the sources one step after the commit that teaches it: that commit, then `make selfcompile`, which replaces the seed, and only then the sources that use it; and branches that teach the seed merge one at a time, each followed by `make selfcompile`.
  - A module moved out of the library keeps its file until `make selfcompile`, since the seed reads the modules of the library that it has compiled in.
  - A new builtin needs the seed first, also one that only the interpreter and the library call, since the C of `make` is that of the seed, whose `native_procs` of `ulang/compile/lower.u` does not know it and calls a mangled name that nothing defines: the builtin goes in with the runtime and `ulang/compile/lower.u` first, then `make selfcompile`, then its uses.
- `make test` is `out/ulang.sh check --test --prove` of the whole project, interpreted, about 35 seconds at a load of 3, where each unit loads and checks its package and the packages it imports.
- `make testc` runs all tests of `out/suite/suite.bin`, compiled by `compile --tests`, about 7 seconds once it is built, with the outcome of `make test` but without the proofs of lemmas; `out/suite/suite.sh ulang/test/errors` runs only the cases of errors. `compile` loads the whole library, so also the module `list`, which the runtime needs.
- `make testelf` runs the tests of the project as `make testc` does, compiled to objects, and `make selfcompileelf` checks that the compiler linked from its object compiles itself to the same object and C, without replacing the seed.
- `make testdocs` builds and runs `checks/docs.u`, about 4 seconds, which checks the references of the documents, the keywords that `doc/language.md` lists, its blocks of u, and the comments of the sources (under Tests of `CLAUDE.md`).
- `make reformat` formats the project, after a change of the printer.
- `make install` packages the VS Code extension of `vscode/` as `ulang.vsix` into VSCodium: highlighting, hover and inlay hints by `ulang.sh hints`, a run button, `▶ Run` links above procedures without parameters, tests, and the lines `example` of functions and procedures, which run the test `f.example.N` of each, numbered as the checker numbers them (`blocks` of `vscode/extension.js`), and `▶ Prove` links above lemmas, errors and open lemmas in the Problems view, and tests in the Testing view, by running the `out/ulang.bin` of the workspace or of the setting `ulang.binary`, so after a change of its path, `make install` again.

The repository is one project, whose root has `project.u`, so that the modules of `ulang/` and of the tests are known by their paths from it, and tests import their helpers by such paths. `project.u` declares what convention cannot decide, the function `project(): Project` of the type `Project` of `ulang/prelude/os.u`, here the directory of programs that run only compiled, `ulang/test/errors/`, which `ulang.sh check --test` does not run when it walks it, read by `compiled_only_of` of `ulang/ulang.u` from the text of `project.u` alone where `project` is the constructor `Project` of a list of string literals (`literal_project`), so that no library is loaded, and else by a load whose library is only declared; no directory below a root may have a file `project.u`, which would be a root of its own.

## Commands at a glance

```
make                                      # builds out/ulang.bin and out/ulang.sh from the sources, by the seed in bootstrap/,
                                          # and copies both to the root, so ./ulang.sh works as out/ulang.sh does
out/ulang.sh run file.u -- args           # checks a program and calls its procedure main
out/ulang.sh run file.u:go                # calls its procedure go instead
out/ulang.sh check file.u                 # checks it: errors of parsing, of types, and of format
out/ulang.sh check --test file.u          # also runs its tests one by one
out/ulang.sh check --test file.u:name     # runs one of them
out/ulang.sh check --test --timeout 10 file.u  # fails each test that runs longer than 10 s
out/ulang.sh check --prove file.u         # also proves its lemmas, which --test --prove does after the tests
out/ulang.sh check --lint file.u          # also prints the lints, as warnings, with simpler expressions to write
out/ulang.sh check --test dir             # the files below dir, which must pass
out/ulang.sh check --test --prove         # the whole project, the root above with project.u, but
                                          # ulang/test/errors/, which project.u declares to run only compiled
out/ulang.sh compile --tests -o suite.c   # compiles the tests of the whole project, whose binary runs at the root
                                          # with the targets of check --test, not its options; --tests dir those below dir, not a file
out/ulang.sh format file.u                # rewrites it in the layout that check requires
out/ulang.sh hints file.u                 # checks it and prints the resolved names and types of its parts and the inferred
                                          # raises, as JSON lines of hover and inlay hints, which the VS Code extension shows
out/ulang.sh hints --overlay file.u=buf.u file.u  # the same with the text of buf.u as that of file.u, an unsaved
                                          # buffer of an editor, as check and refactor take it, also repeated
out/ulang.sh compile file.u -o file.c     # compiles a program to C, whose main calls the procedure main
out/ulang.sh refactor script.txt          # runs a script of edits by declaration, see Refactoring below
out/ulang.sh help                         # lists the commands
cc -I ulang/runtime file.c ulang/runtime/ulang.c -lgmp -lgc
make test                                 # runs all tests
```

## Common to all commands

- The commands are those of `usage` of `ulang/ulang.u`, read by `request` into a `Request`, where a usage error exits with status 2 and prints the usage, also an option that a command does not take (`paths_request`), and `ulang.sh help <command>` describes one.
- Commands exit with status 0 if all succeeds, 1 for errors of the sources, 2 for a usage error, 3 for a run, test, or lemma that fails, and 4 for an internal error, the worst of several, the greatest (`load_status` of `ulang/ulang.u`, `has_errors` of `ulang/parse/parse.u` and of `ulang/check/check.u`, where the parsed modules hold their errors as the parts that do not parse and the checked program as its parts and commands that checking rejects, `AErr`, read by `diagnostics` of `ulang/check/check.u`).
- Only `format` and `refactor` write sources: `check` reports, and the edits that lints propose, `Edit` of `ulang/syntax/syntax.u` in each `Warning` of `ulang/lint/lint.u`, are applied by `refactor @fix`, which a lint never bypasses, while those of errors are planned (Diagnostics 2 of `ideas/diagnostics.md`).
- A target `file.u:name` selects a declaration of a file, where only a path ending in `.u` is split at its last `:`.
- Errors are printed as JSON lines, one `Diagnostic` each, with the fields `file`, `module`, `line`, `column`, `kind`, `code`, `message`, details such as `expected` and `found`, and `severity` `warning` for a warning, which does not fail the command (under Errors of `doc/implementation-notes.md`).
- Errors are deferred: `run` and `check --test` run a program with errors as far as they allow, where a path that reaches a part with errors raises the runtime error `Deferred` (under Errors of `doc/implementation-notes.md`).
- A load takes the packages other than that of its file, the library included, only as far as their importers see them, for the checks that report only the errors and hints of the package of the file: `check` without `--test` and `--prove`, `hints`, and the checks of `refactor`, while `run`, `check --test`, `check --prove`, `compile`, and `@check test` load whole (the cut load of imports, under Modules of `doc/implementation-notes.md`).

## run

`ulang.sh run <file>[:<procedure>] [-- args]` calls the procedure, by default `main`, without parameters, in the interpreter `ulang/run/run.u`, also in a program with errors, after reporting them, as far as they allow; `os_args` gives the arguments after `--`. A missing procedure is the runtime error `UnknownEntry` of `require_entry` of `ulang/check/defs.u`.

## check

`ulang.sh check [--lint] [--test [--timeout <seconds>]] [--prove] [--overlay F=<path> …] [<path>[:<name>] …] [-- args]` loads and checks the `.u` files of the paths, reporting the errors of parsing, of types, and of format, and adds the phases of its options to each unit that loads, also with errors where it is runnable, as far as the errors allow (`check_targets` and `check_unit` of `ulang/ulang.u`).

- **Paths.** By default the root of the project of the working directory, directories walked in the order of names without the hidden ones and those named `out`, and with `--test` without the directories that `project.u` runs only compiled (`targets_of`, `walk_all`, and `compiled_only_of` of `ulang/ulang.u`).
- **Loading.** Without `--test` and `--prove` it loads the packages that a unit imports, the library included, only as far as the unit sees them, so it reports no errors in their definitions, which a check of their own files or `make test` reports, while `--lint` reads only the parsed modules of the unit. The files are grouped into units, the packages of their directories, whose members load together (`units_of`); each unit is loaded and checked once, also for several phases, where the parser keeps the tests of all members of the package of the loaded file.
- **Format.** `check` fails on a file that `format` would change, with a `format error` at the first line that differs, after the errors of parsing and checking (`check_file` of `ulang/ulang.u`, `require_formatted` of `ulang/format/format.u`), also of each member of the unit, so every file that parses and checks is formatted.
- **Output.** A file passes if its unit loads and its phases pass, the status is that of the worst (`load_status`), and with several files `check` prints the files that do not pass, `file: failed` after their failed tests, or the errors of their unit, as JSON lines of its first file, and `N of M files passed`; the errors of a single file that fails to load go to stderr, as JSON lines. Type errors are reported for the first package that has some, at most 20 per file.
- **Names.** Names select tests and lemmas, so a target with a name needs `--test` or `--prove`, and with both, each name selects the test or the lemma of its name, where one that is neither is an error; a file named alone runs only its own tests, though its package loads whole.
- **Overlays.** `--overlay F=<path>`, repeated for several files, takes the text of the file at path as that of `F`, as of an unsaved buffer of an editor, which the loader reads instead of the file, also for the format and the grouping into units, so that the errors are reported against `F` (`overlay_texts`, `visit_units`, and `units_of` of `ulang/ulang.u`, `overlaid_text` of `ulang/parse/parse.u`), as `hints` and `refactor` take it.

### --lint

`--lint` prints the warnings of the lints of each file that loads, as `file: warning[Code]: message, write replacement at L.C`, which do not fail it, where `Code` is the rule, a constructor of `Lint` of `ulang/lint/lint.u`, and a rule without a replacement omits `, write …`. The rules are walks of the parsed module, which keeps the positions and the source that `format` prints, each with a test in `ulang/test/lint/`:

- rewrites from a pattern to a simpler expression with the same meaning (`placed_lints`, `iterated_list`, and `collecting_loops`), of which `BoolIte` skips the claims of lemmas and the conditions of `assert`, whose `==` prints both sides when it fails (`without_bool_ites`), and an `if` or guard whose condition, or `not` of it, written alike, an `if` or guard around it tested, where a statement branch drops the facts of the variables that it changes anywhere (`decided_cmd` and `kept`), printed by `format_module_expr` of `ulang/format/format.u` as `format` writes it at its place, at the indentation of its line (`line_indentation` of `ulang/format/format.u`, `replaced_start` of `ulang/lint/lint.u`) and with the comments of its span, which `@fix` splices;
- locals and pattern variables that are never read, also locals that only receive outs, which `out _` drops, locals declared above the one branching statement that uses them, and a `var` that nothing assigns after its value (`locals_lints`, `declared_lints`, and `binder_lints`);
- private declarations that no export, test, lemma, example, hook, or procedure without formals reaches (`unused_privates`);
- handlers that only `skip` (`swallowed`);
- a part of at least `repeat_threshold` parts of expressions that repeats on one path of a function or procedure, equal modulo positions and the names of the variables bound inside it, which `canon_expr` names by their order, with free variables of the same binders, where tests, the parts in different branches of an `if`, `match`, or `try`, and in a procedure the parts that read a variable that it assigns are not judged (`repeated_exprs`); and a function or procedure with formals whose types and body repeat another of its module modulo the names of the formals, type parameters, and bound variables (`duplicate_definitions`), both without an edit, syntactic versions of R4 and N1 of `ideas/lint.md`;
- names with numeric indices or `show_` (`name_lints`), where a digit after the last `_` of a standard name, such as `utf8` or `md5`, is no index (`standard_names` of `ulang/lint/lint.u`).

The walks judge names as written: a pattern names the library as written, e.g. `Some` and `value_or`, so a module that declares such names again may get a lint that does not check, and declarations count by their base names, so an overload that is used keeps the others.

### --test

`--test` runs the tests of each file, each on its own, in the working directory, so that tests give paths from the root, where `make` runs them, printing `test name passed` or `test name failed: …` per test as it runs, labeled `file: ` if there are several files (`test_module` of `ulang/test/runner.u`), where `os_args` gives the arguments after `--`.

- The tests of a unit with errors run too, failing where they reach one, which a test that fails so names only by its position, `test t failed by reaching file.u:L.C, listed above`, as the reply of `refactor` does (`failure_text` of `ulang/test/runner.u`).
- The interpreted runner reports a failed example by its call, `test gcd.example.1 failed: gcd(12, 18) gave 3, expected 6`, printed from the checked test in the notation of the program (`example_failure` of `ulang/test/runner.u`), while the compiled runner reports `assertion failed: 3 != 6`.
- With `--timeout`, a test that runs longer than its seconds fails with `ran longer than its limit of N s`, by a deadline that the interpreter checks at each call and each round of a `while` (`Deadline` and `require_time` of `ulang/run/run.u`).

### --prove

`--prove` proves all or the named lemmas of each file, after its tests, by the default proof of `ulang/prove/lemma.u` or the proof that a lemma writes, printing:

- `lemma name proved`;
- `lemma name proved, assuming m, which is not established` for one proven locally that rests, by a citation or a fact, on a lemma that is not established, which fails it, with an error of the kind `proof` of the code `Assumed` at the lemma (`assumptions` and `assumed_diagnostic` of `ulang/prove/lemma.u`);
- `lemma name open`, with an error of the kind `proof` for each goal that its proof leaves open, at its step and with the goal as a detail (`gap_diagnostic` of `ulang/prove/lemma.u`);
- `lemma l falsified` for a lemma whose test `l.lemma` finds a counterexample, which is not proven, nor a fact of those after it.

The default proof is simplification, else induction on one of the formals, with the lemmas before it as facts, but those that a test falsified, while a proof cites any lemma whose statement checks, also an open one or one after it, since it looks only at the statements that it cites (`proofs` of `ulang/prove/lemma.u`). A recursive function that the lemmas reach unfolds only if the prover proves that it terminates, else it stays uninterpreted and an error of the kind `proof` names the call that no measure decreases (`termination_diagnostic` of `ulang/prove/lemma.u`). A lemma with errors of its own is not proven, and one whose statement has errors is no citation of the others (`provable` of `ulang/prove/lemma.u`), and none is if code that the lemmas may reach has errors that block a run (`blocks_proofs` of `ulang/ulang.u`). `out/ulang.bin` contains the prover.

## hints

`ulang.sh hints [--overlay F=<path> …] <file>` checks the file as `check` does, with the overlays as those of `check`, without requiring its format, prints its errors to stderr, with its imports loaded only as far as it sees them, as `check` without `--test` loads them, and then prints the findings of the check in its main module to stdout, one JSON object per line in the shapes of `Hover` and `InlayHint` of LSP (`hints` and `format_hint` of `ulang/check/hints.u`, `print_hints` of `ulang/ulang.u`):

- `{"kind": "hover", "line": L, "column": C, "end_line": …, "end_column": …, "text": "prelude.list.size: List[Int] -> Int"}` over each use of a name with its qualified name and its type at the use, and below them the examples of its declaration as written, if it is one of the main module, since imports drop their examples (`examples_of` and `format_hint_with` of `ulang/check/hints.u`), and over each local, formal, and pattern variable with its type;
- `{"kind": "inlay", "line": L, "column": C, "label": ": Int"}` after a declared name whose type the source does not write, unless the type has an unknown, of an error or of a use that stays open, which a hover shows as `?` (`unknowns_hidden` and `has_unknown` of `ulang/check/hints.u`), and `" raises T"` after the formals of a private procedure without `raises`, as inferred;
- columns count code points from 1, and an end is just after the span.

The table is read from the checked program, which keeps the resolved names, the types after the pending uses are solved, the spans, and the inferred raises (`with_inferred` of `ulang/check/check.u`, `Exceptions` of `ulang/check/raises.u`); a part counts only if the parser placed it, which the text tells, a name written over its span, so the parts that translations make at the span of what they translate, and the inner parts of pattern aliases at the spans of their declarations, have none, and neither have the calls of procedure values, whose types the table does not know, while a use of a pattern alias in a pattern, which the checked program keeps, has the type of the alias as a function of its formals (`constr_types` of `ulang/check/hints.u`).

The VS Code extension runs it on the file, with an unsaved buffer written to a file of a temporary directory and passed by `--overlay`, keeps its findings per version of the document, and shows them as hover and inlay hints, refreshed on save and half a second after the last edit of a buffer, while a version before that has none (`parseHint`, `hintsOf`, and `refreshLater` of `vscode/extension.js`).

## format

`ulang.sh format [<path> …]` rewrites the files of the paths, by default the project, as source (`format_program` of `ulang/format/format.u`), in place, and refuses a file whose source does not parse to the same commands. Since `check` fails on a file that `format` would change, every file that parses and checks is formatted, which tests parser and printer by their round trip; `format` keeps the imports in their order, which is the order of loading. Run `ulang.sh format` on each `.u` file after editing it, and `make reformat` after a change of the printer.

## compile

- `ulang.sh compile <file>[:<procedure>] -o <target.c>` translates the program to C, whose `main()` calls the procedure, by default `main`, as the entry `EntryProc` of `ulang/compile/compile.u`. It compiles also with errors that generate no code, those that do not block a run and those of lemmas, tests, and contracts, also of parsing in a lemma or test, which an unparsed command is if the keyword that starts it is `lemma` or `test`, which the parser records in its error (`started_by` of `ulang/parse/parse.u`, `unparsed_command` of `ulang/syntax/syntax.u`), which it leaves out of the program and reports as warnings, while it refuses every other error (`compile_target` of `ulang/ulang.u`, `code_free_errors` of `ulang/syntax/syntax.u`). The C is `static` except `main` and what `ulang/runtime/ulang.h` declares extern (`runtime_referenced` of `ulang/compile/lower.u`), so the C compiler drops the parts of the library that nothing reaches; compile it by `cc -DULANG_LIB='"'$PWD/ulang/prelude'"' -I ulang/runtime target.c ulang/runtime/ulang.c -lgmp -lgc`.
- A target ending in `.s` is x86-64 assembly for Linux instead, by `assemble_program` of `ulang/compile/x86.u` from the checked program; link it by `cc target.s out/asm/ulang.o -lgmp -lgc`, the runtime that `make out/asm/ulang.o` compiles. A target ending in `.o` is an ELF object of the same assembly, encoded by `encode_object` of `ulang/compile/elf.u`, which only the linker needs, `cc target.o out/asm/ulang.o -lgmp -lgc`, the path of the backend, while `.s` is a view for debugging.
- `ulang.sh compile --tests [<dir>] -o <target.c>` translates the tests of all `.u` files below the directory, by default the root of the project, with its packages and subpackages, also of the directories that `check --test` skips, into one program, and refuses a file, each unit checked as `check --test` checks it and each module taken from its own unit (`compile_tests` of `ulang/ulang.u`), whose `main()` takes the targets of `check --test`, files with their names and directories, and the arguments after `--`, but none of its options, not `--timeout`, at the root, and runs the selected tests as `check --test` does, with the tests of lemmas but without their proofs (`EntryTests` of `ulang/compile/compile.u`, `u_run_tests` of `ulang/runtime/ulang.c`); a unit that does not load fails the compilation.
- The runtime sets the free-space divisor of the collector to 1, which collects less often than the default 3 at a larger heap, unless `GC_FREE_SPACE_DIVISOR` of the environment is set, e.g. to 3 by a session short of memory.

## Refactoring

`ulang.sh refactor [--dry] [--overlay F=<path> …] [<script> | @<action> …]` edits sources by their meaning, by declaration and node, as `format` edits their layout. Prefer it to text edits of `.u` files: a script is one transaction, writes formatted files, and replies with the state and the follow-ups of the change.

### Scripts

- A script comes from a file, from stdin, or as actions on the command line, a line of the script from each word that starts with `@`, `out/ulang.sh refactor @show shapes.u:area @refs shapes.u:area`, where the last one, if it is a `@put`, `@patch`, or `@example`, takes its fragment from stdin (`inline_script` of `ulang/ulang.u`).
- Each action is a line that starts with `@`, which ulang never uses. Its fragment is the lines up to the next action, taken verbatim, where a line that starts with `@@` is a line of the fragment with one `@` (`is_action_line` and `fragment_lines` of `ulang/refactor/refactor.u`), which a fragment of `@patch` never needs, since its lines start with `-` or `+`; so an action that takes a fragment and has none, since the line after it starts with `@`, is refused with the line that it took as an action, and an unknown action with the escape `@@`.
- A script writes all its files or, if one action is refused, none. It splices the text of whole declarations and nodes by the spans of the parsed file, since comments are not in the tree (`run_script` of `ulang/refactor/refactor.u`). Fragments need no layout, since every edited file is printed by `format`.
- A script edits files that parse, also with an import that fails, such as one of a package that moved, which a `@put` of the new import replaces.
- The actions and the checks of a script see the files that it has written so far, also a module of a new package, which another file of the same script may import: its parses and checks read the texts that the script has edited as overlays, which the loader takes instead of their files, also in directories that do not exist yet, so that a new package and its first import are one script (`parse_file_overlaid` and `overlaid_files` of `ulang/parse/parse.u`, `parsed` of `ulang/refactor/refactor.u`). They load the imports only as far as the file sees them, and the parse of the file of each action reads only the package of the file and the export and builtin sections of the imports.
- Paths in actions are normal, `./a.u` is `a.u` (`normal_words`), and the handles are kept by their paths from the root, so that they hold from any directory.
- `--dry` writes nothing, but checks the texts that it would write, so its reply, `dry, 2 errors (2 new)`, lists the diagnostics that the script would leave, without handles, since their nodes are in no file, and without tests, which run on the files.
- `--overlay F=<path>`, repeated for several files, takes the text of the file at path as that of `F`, as an editor passes its unsaved buffers, for the actions and both checks; the script writes `F` only if it changes it (`refactor_request` of `ulang/ulang.u`).
- The journal of `@undo` is `out/refactor/journal/` and the handles are `out/refactor/handles.txt`, both in `out/refactor/` of the root (`state_dir`); the queries write nothing but the handles they show.
- Its scripts are tested in `ulang/refactor/test/`, which `check --test` skips when it walks the project, since `project.u` runs it only compiled, as each parse loads the library, while a file there named to `check --test` runs interpreted, far too slowly; the rest of `ideas/refactor.md` is planned.

### Nodes and handles

`@show` prints code with handles, `#n` in a margin beside each line that starts a node: a statement, a case, a constructor, or an expression that starts its line, the body of a function, the body of a `let` after `in`, a branch of an `if` spread over lines, an element or argument of a form or call spread one per line, or a line of a spec, an example, `requires`, or `ensures`, where those of a signature in `export` are nodes of its definition, and the head of the definition of a function or procedure and of each of its signatures in `export`, up to its `:=`, which a diagnostic at the head, such as `SignatureMismatch`, names, and which only `@put N` replaces, where the signature in `export` follows the new head of a definition, while a head of a signature is replaced alone and a head of another name or arity moves the declaration with its handles to its key, `replaced F:p/3 #4, now p/2` (`kinded_nodes`, `spec_nodes`, and `head_nodes` of `ulang/refactor/nodes.u`, `decl_of` of `ulang/refactor/refactor.u`). A node is its text from its first token to its last, so what follows it on its last line, such as the comma after an element or the `)` after the last one, stays where an edit replaces it (`span_splice`); `@show` of a node and the echo of an edit also print the comments that `format` places above it, which an edit of the node keeps (`with_comments`).

```
$ out/ulang.sh refactor @show shapes.u:area
applied
@show shapes.u:area
      // the area, rounded
#1    function area(s: Shape): Int :=
#2      match s
#3        case Circle(r) =>
#4          3 * r * r
#5        case Square(side) =>
#6          side * side
        end
```

- Handles are kept by the index and the hash of the text of each node. A handle stays valid across edits of other declarations, and across the edits of the same script, also of `@patch`: the edits of a script move those of their declaration to its new nodes, at their places where `format` moved nothing around the edit, also among identical statements, and a node replaced by its own text keeps its handle and those inside it (`rebase` and `aligned`). It becomes stale when its node is changed by something else, an edit from outside, and `@show` gives a new one. An edit that removes nodes says so, with the handles of the nodes that it placed, `gone #4, #5 (replaced by #10)` (`gone_text`).
- A node N is a handle `#n`, a handle scoped by its declaration `F:name #n`, which is refused if the handle lies elsewhere, the node of a declaration that starts with a text, `F:name "case Listing"`, or the node at a position, `F:L.C` (`resolved_address` and `node_by_text`). The text is all between the first and the last quote of the line, read with the escapes of string literals, `F:name "case List(\"fetch\")"`, and else as written, so that `F:name "case List("fetch")"` names the same node (`unescaped`).

### Targets

A declaration is a target `F:name`, or `F:name/n` of the arity `n` among overloads, or `F:name/test`, `F:name/lemma`, or `F:name/type` of that kind, which has no arity, so that a procedure `coverage/4` and a test `coverage` are told apart; an action that takes one declaration refuses a target that names several, with each of them, its kind, and the target that names it alone, `@patch takes one declaration, while F:coverage names 2: the procedure coverage/4, which F:coverage/4 takes, the test coverage, which F:coverage/test takes` (`target_of` and `several_text`). A target may also name its module instead of its file, by its path from the root or of the library with `.`, `ulang.check.check.infer/3` or `prelude.str.trim`, a constructor by its datatype, `shapes.Shape.Circle`, and a field, which `@refs` takes, `shapes.Shape.r` or `shapes.u:Shape.r`, written as targets of the file before an action other than `@sig` takes them (`module_words`); a local, formal, or pattern variable is `F:proc/local`, which only `@resolve` takes (`local_target`, `local_refusal`, and `resolve_local`).

### An example and its reply

The script below adds a constructor and its case, then applies the edits of the lints, and runs the tests:

```
@put shapes.u
data Shape :=
  case Circle(r: Int)
  case Square(side: Int)
  case Rect(width: Int, height: Int)
@put into #2
case Rect(width, height) => width * height
@fix shapes.u
@check test
```

```
applied, clean, 1 test passed
shapes.u: replaced Shape
put into shapes.u:area/1 #2
#7        case Rect(width, height) =>
#8          width * height
shapes.u: fixed BoolIte at 20.3
script 1 of the journal
checked 1 file: 0 new, 1 gone
  - shapes.u:15.3: warning[BoolIte]: an if of true and false is its condition, write area(s) > 100
tested shapes.u: 1 passed, 0 failed
```

- **The state.** The first line of the reply is the state after the script, `applied, clean`, or its errors and warnings, each with how many of them the script made new if it changed a file, `applied, 2 errors (1 new), 1 warning (0 new)`, and its tests.
- **The actions.** Then what each action did, with the nodes that an edit by handle placed.
- **The diagnostics.** Then the diagnostics and the warnings of the lints that are new (`+`) or gone (`-`) in the files that it checked, the format of every member of their units included, each new one with the handle of its node, once per file, position, and code (`deduplicated` of `ulang/refactor/refactor.u`), where one that only moved, by the same lines as those that its message names, is neither new nor gone (`is_same` and `relative_lines` of `ulang/refactor/refactor.u`). Without the `@put into #2` above, the reply would have listed the match that the new constructor leaves open as a new diagnostic, so the follow-ups of a change are a list of work, not a search.
- **What it checks.** It checks the files that it changed and those of `@check`, each with the packages that it imports only as far as it sees them, so without the errors in the definitions of the imports, which only a script that checks a file of that package lists (`checked_diagnostics` and `format_errors` of `ulang/ulang.u`), but not the importers of a changed package: a script that changes the declarations of an export or builtin section names them in a line of their own, read from the texts, `a/a.u: the exports f changed, whose importers are not checked`, since those may now have errors, where a changed datatype names itself, the constructors whose lines changed, and the fields whose types or positions among the fields of a constructor changed (`changed_exports` of `ulang/refactor/refactor.u`).
- **Long comments.** It names each comment line that it wrote longer than the 120 columns to which comments are filled, which `format` keeps as written, `a/a.u:3: a comment line of 152 characters, longer than 120, which format does not wrap`.
- **Tests.** A test that fails by reaching an error of the reply says so instead of repeating it, and the tests and examples that errors keep out of the program are counted as not run, as are the examples on a definition that belong to its signature, which `ExampleOnDefinition` reports, while the lemmas whose tests they keep out are named.
- **Status.** The status is 0 if the files are clean or have warnings only, 1 if errors remain or the script is refused (`rejected`, with the message of the refusal, and for an error of the system, such as a file that cannot be read, the line and the action of the script), and 3 if a test failed.
- **Errors in fragments.** An error in a fragment names its line in the fragment and in the script, `fragment 2.6, script 3.6`. Where the fragment has an error, the errors at the lines of the file, which parsed before, are its follow-ups and left out, as are those after a keyword written as a name on its line, which the parse took as the start of a section or a command: a field `builtin` replies only `fragment 2.29, script 3.29: keyword builtin is not a name`.
- **Corrections.** Where the error is a parse error of a habit of another language that has a mechanical correction, read from the message and its hint, `x += e`, `return` in a function, `[1, 2]`, modes out of order, a keyword as a name that the hint names a name for, and `elif` or `else if` without an `end` of its own, the reply lists the corrected lines of the fragment, `corrected by the hints, which @retry applies:` and `fragment 3, script 4:   x := x + 2`, also for the new lines of a `@patch`, and keeps the corrected script in `out/refactor/retry.txt`, which `@retry`, alone in its script, runs and each script that writes empties (`corrections`, `correction_lines`, and `corrected_script` of `ulang/refactor/habits.u`, `retried` of `ulang/refactor/refactor.u`).
- **Refused queries.** A query that is refused, `@show`, `@outline`, `@refs`, `@sig`, or `@resolve`, takes no other action down with it, since it writes nothing: the reply says `@show T: refused, …` in its place, its first line counts it, `applied, clean, 1 query refused`, and the status is 1, while any other action that is refused refuses the script (`is_query`).

### Queries

Four queries tell what a module offers, what a name is, and who uses it, before an edit:

```
$ out/ulang.sh refactor @outline shapes.u
applied
@outline shapes.u
1-4   none data     Shape    private  #9 cases Circle/1, Square/1, Rect/2
7-17  none function area/1   private  #2
19-20 none function is_big/1 private  #10
22-23 none test     areas    private  #11
$ out/ulang.sh refactor @refs shapes.u:area
applied
@refs shapes.u:area: 2 uses of shapes.area/1 in 1 file
  shapes.u:20.3 in is_big/1 #10: area(s) > 100
  shapes.u:23.10 in areas/0 #11: assert area(Square(3)) == 9 and not is_big(Circle(2))
$ out/ulang.sh refactor @sig lines
applied
@sig lines: 1 declaration in 1 module
prelude.str.lines:
  function lines(s: Str): List[Str] :=
    example lines("a\nb\n") == List("a", "b")
    example lines("a\n\nb") == List("a", "", "b")
    example lines("") == List()
$ out/ulang.sh refactor @resolve shapes.u:20.3
applied
@resolve shapes.u:20.3: use of shapes.area: Shape -> Int
```

They are `outline`, `refs`, `name_sig`, `module_sig`, and `resolve` of `ulang/refactor/refactor.u`, where `@sig` of a name parses the files of the project and the library whose export sections name it outside comments, each with its package and imports unless an earlier parse loaded it, and `@resolve` and `@refs` read the table of `hints` of `ulang/check/hints.u`, loading each file that they search as the main one (`file_hints` of `ulang/refactor/refactor.u`).

### Actions

| Action | What it does |
|---|---|
| `@show T` | prints a file, a declaration `file.u:name[/arity]`, or a node N, with handles |
| `@outline F` | one line per declaration of `F`: its lines, section, kind, name and arity, whether exported, its handle, and the constructors of a datatype |
| `@sig name` | the exported declarations of the name in the modules of the project and the library, across overloads, each by its qualified name: a signature with its raises and its examples, which it leaves out beyond 4 lines, a datatype whole, and a constructor under the head of its datatype |
| `@sig M` | the export and builtin sections of a module, a file `F` or a path from the root, `ulang/check/check`, or `prelude/str` of the library, as the file writes them, with their docs |
| `@resolve F:L.C`, `@resolve F L.C` | the qualified name and type of the use at the position, or the type of the variable declared there, from `hints` |
| `@resolve F:proc/local` | the declarations of the local, formal, or pattern variable in the declaration, each with its type and position, and the positions where it is read |
| `@refs F:name[/n]`, `@refs F:T.f` | the uses of a function, procedure, pattern alias, or constructor, or the accesses and updates of the field `f` of the datatype `T`, by the names `T.f` and `T.f:=` of the hints, in its package and its importers, by resolution, each with its position, declaration, handle, and line |
| `@put F` | writes the declarations of the fragment into `F`, each replacing the one of its name and arity where it stands, whatever section the fragment names, or added, which the reply says with its section, naming each declaration once, and the other arities of its name that the file declares, which it overloads, `added g/0 to implementation, beside g/1`, and each import that it added, `added import p.q`, and says `of another type` where a replacement's types differ; a missing or empty `F` is the fragment, header, imports, and sections, in a directory made if missing, which the loader then takes as a package of no other members (`package_files` of `ulang/parse/parse.u`); into a file, the `import` lines join its imports and the declarations after `export` or `builtin` go into that section, which is made if missing; an import of the fragment replaces an import of the file of the same last name that fails, as of a package that moved, a declaration keeps its doc unless the fragment writes one, the signature in `export` follows the head of its definition and, if the fragment writes no signature, takes the doc of the definition, unless the file's definition has a doc of its own, which the fragment's then replaces, or the definition is a datatype, whose abstract type in `export` keeps its doc, for its importers, and its examples and contracts, in place of those of the signature, `the examples of f/1 on its signature`, a definition follows the head of a signature in `export` that the fragment writes without the definition, also of another arity and of a private definition that it exports, `with the head of the definition of f/1`, a private procedure that the fragment exports gets the raises of its signature or else those that the check inferred in both heads, as `@export` does, and a comment of no declaration is refused (`put_into`, `put_anchored`, `with_definitions`, and `with_exported_raises` of `ulang/refactor/refactor.u`) |
| `@put F:name[/n]` | the declaration of that name in the fragment replaces the target at its place, whatever its arity, with its signature; of the others, each that the file declares by its name and arity replaces it where it stands, as for `@put F`, and the new ones follow the target in their sections, in the order of the fragment, also one of a name that the file declares with another arity, which is added beside it as an overload, each named by its place, after the target or the one added before it, `added g/0 after f/1, beside g/1`, so that a declaration of the old arity is dropped by `@drop F:g/1`, while one that replaces a declaration other than the target says where that stood, `replaced h/0 at line 7, which @put F:f does not target`, so that a helper of the fragment that meets a declaration elsewhere in the file is told from the additions, rather than refused, since replacing a helper together with its caller is the common case and `@undo` takes it back; a fragment of one declaration of another name, of the target's kind, replaces it so too, while the uses keep the old name, which `@rename` changes |
| `@put [F] before T`, `@put [F] after T` | places the new declarations of the fragment next to the declaration `T` in its section, which the reply names, `added b/0 after a/0`, where the lines under no section go into the builtin section if `T` is a builtin, and into the export section if they are signatures or abstract types, which only it takes, and `T` has a signature there, where `T` is `F:name[/n]`, so `@put F after name` is refused with the form `@put after F:name` |
| `@put N`, `@put before N`, `@put after N`, `@put into N` | replaces the node, or inserts code next to it or as its last statement or case, at its indentation, refused for a fragment of another kind than the node, a statement, a case, an expression, a constructor, or a head, which nothing goes next to or into, or one that writes declarations; a fragment may keep the margin of `@show`, and writes the node alone, without the comma or `)` after it, which stay, while one before or after an element gets the comma between them, also after the last one, and a drop of the last element moves its `)`, where a comment at the end of a line stays with the element that it follows; a drop of the last line of the spec of a signature drops its `:=` |
| `@drop T` | removes a declaration, with its doc and signature, saying the sections where it had more than one, or a node N |
| `@patch T` | edits the text of a declaration, its signature and definition alike, of a node N, or of a file `F`, by hunks written as in a diff, see Patching below |
| `@example F:name[/n]` | adds the examples of the fragment, lines `example …`, after the spec of the function or procedure, on its signature if it has one, the place of the examples of an exported name, else on its definition, and shows them with their handles (`add_examples`) |
| `@rename T new` | renames a function, procedure, pattern alias, or constructor at its declarations and at every use that resolves to it, several per script, with a warning where the new name meets declarations of any arity that the uses also see; it renames no types, fields, locals, or modules yet |
| `@export T`, `@unexport T` | moves a definition's doc, contract, and examples to a new signature in `export`, beside the signature of the exported declaration next to the definition, with the raises that a procedure infers, or back, refused while another module uses it; a datatype or an alias moves whole (`export_target` and `unexport_target`) |
| `@fix T [Rule …]` | applies the edits of the lints of a file or a declaration, all or of the rules named |
| `@undo [n]` | restores the files of a script of the journal, by default the last one |
| `@retry` | runs the corrected script of the last refusal, alone in its script, whose reply listed the corrections; each script that writes replaces it |
| `@check [test] [F]` | checks `F` too, and with `test` runs the tests of the files, whose results the reply lists, while what the tests print goes to stderr, so that the first line of the output is the state (`print_all` of `ulang/run/run.u`), but not those of a directory that `project.u` runs only compiled, whose files it names (`compiled_only_files` of `ulang/ulang.u`), and names the tests of their packages that the errors of an earlier script left out and that no script has run since, kept in `out/refactor/unrun.txt`, `not run since an earlier script, whose errors left them out: a/wire.u: test round_trip` (`refactor_tests` of `ulang/ulang.u`, `test_outcomes` of `ulang/test/runner.u`, `run_actions` and `load_unrun` of `ulang/refactor/refactor.u`) |

- `@rename` finds the uses by the names that the checker resolved, by `hints` of `ulang/check/hints.u`, not by their text, so it never renames another overload or a local. It writes the edits of the script before it and its own at once, then checks every file of the package and every importer that contains the name, and undoes the script if any use resolves differently, e.g. to a local of the new name (`flush` and `commit_rename`). It lists the mentions in `CLAUDE.md`, `README.md`, the files of `doc/`, and `ideas/` that it leaves to you (`doc_references`).
- `@fix` rewrites expressions, each written as `format` writes it at its place, at the indentation of its line and with the comments inside it, as `check --lint` shows it, drops an unused private declaration, writes `_` for an unused pattern variable and at the outs that only feed an unused local, removes an unused local, makes a `var` that nothing assigns a `let`, and moves the declarations of a `var` that nothing assigns into a `let` above it where the others are assigned, and moves a local that is declared too high into the branches that use it. A fix that enables another takes a second `@fix`. An edit after which the unit of the file has an error that it did not have before is refused and listed in the reply with that error, `f.u: refused OptionMatch at 5.3, which would cause f.u:5.3: …`, while the others apply: the edits are checked together, and if that adds an error, each in turn on top of those taken (`fix`, `tried_splices`, and `local_splices` of `ulang/refactor/refactor.u`).

### Patching

`@patch T` takes hunks as a diff writes them: a run of lines that start with `-` is an old text, and the run of lines that start with `+` right after it is its new text. The marker and one space after it are stripped, and the rest of each line is taken as written, `=>` and all, so a line of a `case`, a lambda, or a map patches as any other (`patch_hunks` and `patch_marker`):

```
@patch shapes.u:area
- case Circle(r) =>
+ case Circle(radius) =>
-3 * r * r
+3 * radius * radius
```

- **Hunks.** A blank line ends a hunk, a `-` run without `+` lines deletes its text, and a line that starts with neither marker is refused with the form. The hunks apply in order, each to the text that those before it left (`patch_hunk`), so a hunk may anchor on what one before it wrote; applying all against the original would make two hunks of one text conflict.
- **Matching.** The old lines are compared without their indentation, as whole lines, or, for a hunk of one old line, as a part of a line; of several old lines that occur nowhere as whole lines, the first may be the end of its line and the last the start of its line, whose rest stays around the new lines (`block_matches` and `replaced_lines`). An old text occurs in the target, and at most once in each of its parts: the signature and the definition of a declaration, so that a text of both heads changes in both, or each declaration of a file and each run of its lines of no declaration, the comments above the imports included. Old lines of a file that span declarations are matched once in the whole file (`file_parts`).
- **Regions.** A line `-...` or `-…` stands for the old lines between those before it and after it, the nearest after, and a line `+...` for where they go, so a region is wrapped in a block without repeating it, which two patches cannot do, since each action must leave the declaration parsing:

  ```
  @patch shapes.u:p
  -y := y + 2
  -...
  -z := y
  +if y > 0 then
  +  y := y + 2
  +  ...
  +  z := y
  +else
  +  z := 0
  +end
  ```

- **Every occurrence.** A hunk of one line `-* x` replaces every occurrence of `x` in the target by its one `+` line, or deletes them without one, and the reply says how often, `replaced "x" 4 times`; an old line that starts with `*` is written `- *`.
- **Refusals.** A hunk whose new lines are its old ones, and a patch that changes nothing once formatted, are refused. A hunk whose old text occurs nowhere or too often says so, by its fragment line where the patch has several hunks, with the parts where it occurs, the hunk before it that changed it, the earlier action of the script that removed it from the target, the declarations of the file that have it, the earlier action that removed it from the file, and else the line of the target that starts most like it (`absent_context`). A patch whose text does not parse is refused as `@patch T writes a text that does not parse`, with the fragment lines of its new lines.
- **Reply.** A patch of a file names the declarations that it changed. The reply shows the nodes that the patch changed, with their handles, those of each run of changed lines, so that the hunks of a patch show their own nodes and not those between them, where a changed comment counts as the node below it, which it documents, and a changed line of a `let … in` as the `let` without its body, unless the body changed too (`changed_nodes`).
