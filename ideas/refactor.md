# Refactor: scripted editing of ulang files by `ulang.sh`

The open items of the command `refactor`, from its uses, and the design that they come from, with the operations that the history of this repository shows agents doing by hand; the refactorings in general are Diagnostics 4, and the queries `resolve`, `refs`, `sig`, and `names` on the command line, with the record of resolutions behind them, Diagnostics 3. It takes in the second version of `edit-tool.md`, now removed, the design of an agent that wrote about 4,100 lines of ulang in another project on 12–13 November 2026: the interface of `refactor` is a script of actions that writes code by declaration and reports follow-ups as diagnostics with handles (Edit scripts, below), and the named operations of this file are actions that come later, where that loop proves too slow. What is built is described in Refactoring of `README.md`, and an item that is done is deleted here.

## Status

Built, on `main`, see Refactoring of `README.md`: `@show` and `@outline`, `@refs`, `@put` of declarations, of new modules, imports, and exports, before or after a declaration or in its place whatever its arity, and at nodes, `@export` and `@unexport`, `@drop`, `@patch` of lines and of blocks, `@rename` of functions, procedures, pattern aliases, and constructors, several per script, `@fix` of the edits that the lints carry, but for `CollectingLoop`, which rewrites statements, and `SwallowedException`, whose fix is a choice, `@undo` with a journal, and `@check [test] [F]`; a node is a handle, also scoped by its declaration, a text that starts it, or a position; the reply states the state first, `applied, clean` or its errors, warnings, and tests, exits 1 while errors remain and 3 if a test failed, and lists the diagnostics and lints that are new or gone, each with a handle. Items 1 to 12, 15 to 21, and 23 of `13cfb89` were done on 15 November 2026 by four agents in parallel, replies, handles, queries, and declarations, each with a test file of its own in `ulang/refactor/test/`; the numbers of items in the paragraphs of uses below are those of `13cfb89`. Open are the items below, then the rest of this file, which is the design that they come from, marked where it differs from what was built.

What the prototype showed:

- Comments are not in the tree, so a doc survives an edit only because the edit splices text by the spans of the declarations, which `format` then normalizes, and the head of an exported signature is taken from the formatted file, since a fragment may write its head and body on one line.
- A change of arity added a declaration rather than replacing one, so adding a formal was an `@put` and an `@drop`, until `@put F:name` replaced the target whatever its arity.
- Handles cannot follow their nodes by lines, since `format` moves lines around an edit too, e.g. the blank lines between cases once a match has three, so an edit keeps the handles of the nodes before and after it at their places where `format` moved nothing else, and else moves them by the text of each node and, for the nodes around the edit, by their paths in the tree of nodes (`rebase` and `aligned` of `ulang/refactor/refactor.u`).
- `hints` of `ulang/check/hints.u` is the record of resolutions for functions, procedures, and constructors, but has no uses of types and none of fields at their names, which their rename needs.
- Timing: an `@put` into `ulang/check/check.u` takes about 2 seconds, with the check before and after about 2.7; a rename checks each member of the package and each file that imports it, which name the word, before and after the writes, about 25 seconds for `parse_diagnostic` of `ulang/parse/parse.u` in 3 files; the scripts of a client of 560 lines ran in 0.8 to 1.6 seconds each.

Uses so far, whose points are the items below:

- A trial by an agent that knew only the docs, three edits of `ulang/parse/parse.u` at their first attempts; its points on the reply, on the docs, and on `@rename` and the docs are fixed, and its move of a local in two `@put`s is now one `@fix`.
- A mail client of about 560 lines in five modules, POP3 and SMTP, written through `refactor` by an agent on 15 November 2026 against `494eeff` (its `REPORT.md`, R1 to R20 below), and the same project of about 650 lines replayed from the scripts of a stand-in driver (its `TRIPUPS.md`, T below). Both found the core loop right: `@put` of whole declarations, follow-ups as diagnostics, `@show` then `@put` by handle, `@rename` by resolution, also into the importers of the package, which the stand-in by text got wrong, `@fix` of `LocalTooHigh`, and `@undo`, which rescued two mistakes. A patch line split at each ` => ` in turn, which T met mangled in the stand-in, is refused where several splits match. They left the tool only for what it could not write, files, sections, imports, and exports, which `@put`, `@export`, and `@unexport` write since.
- A second mail client, 809 lines in six files, POP3 and SMTP over a stream on stdin and stdout, written through `refactor` by another agent on 15 November 2026 against `b86cc59`, which had read the items of this file first (its trip-ups, 2T1 to 2T12 below, and its report, 2R): 30 scripts, 15 seconds of tool time in all, the slowest 1.1 seconds, 20 of them with status 0, 5 with 1, and 5 with 3; three modules passed their tests on the first script that wrote them, a move across files with exports was one script, both renames found their uses in importers and left the word in comments alone, a new constructor gave one follow-up at the right match, and `@undo` took back a wrong `@drop` in 0.2 seconds. Its seven edits by hand were all for writing modules, imports, and exports, done since, and it confirmed items 5, 7, 9 to 15, and 22, and others done since. Its order of what to fix first: writing modules, done, the silent losses (items 3 and 4, and one done), handles to trust (items 6, 7, and 12), and the first line and the repeats (items 9, 10, and 14), then `--dry` with a check (item 13) and examples of an existing signature, done by `@patch` of a declaration; it treated every handle as valid for one script only and ran `@show` before each edit by handle, which doubled the calls, and stopped reading the first line of a reply for the exit status. Its review of its own record (its `WORKFLOW.md`, 2W): the seven edits by hand were each by shell outside any declaration, `project.u`, four skeletons of a header, an import, `export` with one datatype, and `implementation` with one function, an empty `mail.u`, and the header and imports of `mail.u` that a script dropped; ten reads of files outside `@show`, nine of which asked what is in which section and in which order, since a reply then named the declarations it added but not their section, and one of which, not a reply, showed that `@drop` had removed both copies of a datatype (item 3); so in the first half, while the modules got their shape, it read whole files as often as with text edits, and only from its 16th script on by `@show` alone, about fifteen calls. Its commands missed, by what each would have saved: `@put F` that creates a file with its header, imports, and sections (done, all seven edits by hand); `@export` (done, six scripts of the workaround, each confirmed by a read); an outline (item 1, nine of the ten reads); a position as a target (item 12); `@uses` (item 2); a replace of another arity at its place (item 23); a node by its text (item 8); `@example` (item 24); `@import` (done, by the `import` lines of `@put`); and `@check` that prints the state (item 9); once each `@move`, which `@put` and `@drop` did well enough, and `--dry` that checks (item 13); not missed: the multi-line `@patch` (item 20) and several renames per script, done. Its judgement: its actions are the right ones inside a declaration, and what they miss is the module as a thing to write, file, header, imports, sections, and export, and the questions before an edit, what is here, where is this position, and who uses this, the queries that this file had listed as not yet asked for.

## Open items

By priority, each the problem and the solution; the causes named are read from the code, not yet confirmed by a test.

### Checks

1. **`--dry` does not show what would break** (R18, 2T): `--dry @drop wire.u:trim` of a function that three others use replied `would write wire.u` and nothing else, since the check reads the files from disk, which a dry run does not write; so did a dry run of a script with 13 errors. Solution: check the texts in memory, which needs `load_file` to take the edited texts of the unit instead of reading them, the overlays of Concurrent edits 7.
2. **Cascades in the follow-ups of a datatype** (R14, T, 2T): 17 diagnostics for 6 sites after a change of the fields of a constructor, and four for each call of a procedure of the old arity. The messages are Diagnostics 6; they cost in the loop of follow-ups, where an agent reads every diagnostic of a reply to find the few sites to change.
3. **The tests of lemmas are not among the tests that did not run**: the reply of `@check test` counts the tests and examples that errors kept out of the program, but not the tests `l.lemma`, since whether a lemma gets one depends on whether the types of its formals are enumerable, which the source does not tell. Solution: count them from the generated tests of a check without errors of the unit, or name the lemmas whose tests were not generated.

### Actions

4. **Fields, types, locals, and modules are not renamed** (R17, Status, 2T): `@rename mail.u:Request.Send.file path` is refused, clearly, and so was `@rename smtp/smtp.u:Envelope.host client`; nor does `@refs` take them. Solution: the record of resolutions of Changes to ulang 1, with uses of types and of fields at their names, which `hints` lacks.
5. **Examples have no handles and no action of their own** (2W): `@patch F:name` reaches the examples on the signature of an export, but the lines of a spec are no nodes. Solution: the examples, contracts, and other lines of a spec as nodes with handles, and `@example F:name` of a fragment of examples that it adds to the spec of the declaration, wherever its spec stands.
6. **`@patch` takes no node by its text or position**: `@show`, `@put`, and `@drop` take a node as `F:name "text"` or `F:L.C`, `@patch` only a handle or a declaration. Solution: the same forms of a node for `@patch`.
7. **A file spelled two ways has two sets of handles**: the handles of a diagnostic are matched by the path as given, so `./x.u` and `x.u` give two handles of one node. Solution: the paths of handles normal, from the root.
8. **The warning of `@rename` of a name that the uses also see lists only declarations of the same arity**, and its refusal for a use that would resolve differently can name the wrong side, `a use of s would resolve to prelude/str.lower`, where `s` is the formal that the new name `lower` met. Solution: list the declarations of the new name of every arity that the uses see, and name the use by its text and position.
9. **`@refs` repeats the choice of files of `@rename`**, the members of the package and the files that import it and name the word, in a copy of its own. Solution: one procedure of both.

Planned from the design and not yet asked for by a use: `@retry` of corrected fragments, `@check prove`, `--json`, the placing of locals in fragments, which `@fix` of `LocalTooHigh` does after them, replies such as `gone #9 (replaced by #31)`, the queries `resolve`, `sig`, and `names` of Diagnostics 3, the operations but `move`, which the second client would have used once, and the lock and replay of Concurrent edits.

## Why ulang is unusually suited to it

- **`format` is canonical and `check` enforces it**, so a tool that prints every edited file by `format_program` produces a diff of exactly the changed parts, and no layout is lost.
- **The checked program keeps every position** (`At` since `1c2db95`) and resolves every name to its module, so the qualified name and type of any use are found by its position; overloading, which defeats `grep` (`size` in a file that imports `set` and `map`), is no obstacle once the checker is asked.
- **The parser desugars nothing**, so the tree that is rewritten is the source as written.
- **The checks are strong** (exhaustiveness, `HiddenLocal`, definite assignment, `raises`), so "check again after the edit" catches most broken refactorings, and the remaining risk is a use that silently resolves differently, which the tool can rule out by comparison (below).
- **A declaration names itself**: within a module, overloading is only by arity, so a fragment `function emphasis(…) := …` says which declaration it replaces by its name and arity, and an edit of whole declarations needs no address.
- **`@` occurs nowhere in ulang**, so a script can delimit fragments by lines starting with `@` and take them verbatim, without escaping.
- **Docs reference code in a checked grammar** (`` `name` of `file` ``, `checks/docs.u`), so `@rename` lists the mentions in `CLAUDE.md`, `README.md`, and `ideas/` (`doc_references` of `ulang/refactor/refactor.u`), and moves and renames could rewrite them, which Code 4 demands and agents forget; planned.

## What the history shows done by hand

Each of these was a commit of many mechanical edits, several with regular expressions, one of which went wrong (unclear.md: `(` and `)` as boundaries skipped every name in an argument list):

- renames of fields by role, with patterns rebinding the new names (`2543f3f`), of constructors (`Failure` to `RuntimeError`, `GFor` to `GVal`, `Elem` to `Yield`), of a package (`self` to `ulang`, `f7dc044`), of functions (`alias_of` to `type_alias_of`), of locals that hid others (`32f41e2`);
- positional tuples to records with selectors (`8e97aed`);
- pattern aliases replacing spelled-out shapes and guards (`2ab6b2f`);
- moves of declarations between modules and of tests between files (`26c9c42`, `64a4092`), and the split of `ulang/` into a package per phase (`ff02be2`);
- functions turned into procedures when they had to raise (`8084d78`: `as_type`, `type_apply`), and `raises` written into the exported signatures (`9335b69`);
- 211 failing files turned into cases of `ulang/test/errors/` (`15b55d4`);
- the expansion of `case _` into named constructors, mostly done, the rest Code 1, by the alternatives of `ideas/or-patterns.md`;
- exports added because another member needed a private helper (`covers_all` and `strip_yield`, both exported since).

## How an agent edits

From that session:

1. It writes whole declarations as text, and replaces a whole function more often than it changes a line in it.
2. It batches several changes, then checks once.
3. Its failed edits were text matches against code that `format` had reshaped, or edits by line number after earlier edits had moved the lines.
4. The expensive part is the follow-ups: after a change of a datatype or a signature, finding every site that has to change too.
5. It already uses the diagnostics as a list of work: change, check, fix what is reported, until nothing is.

So the tool makes this loop safe and cheap before it adds a vocabulary of refactorings, which is much to learn for an agent with almost no ulang in its training, and against the principle of one way of each thing. The first version of that design had about 40 actions, one per refactoring, as the operations below are; the second has a few that write code by declaration, of which ten are built:

| | Named operations | Edit script |
|---|---|---|
| Actions | about 40, one per refactoring | the ten of `README.md`, plus the planned `@retry` |
| Code | in arguments, by operation | in fragments, by declaration, which names itself |
| Follow-ups | each operation updates its sites | the checker reports them; mechanical ones applied, planned |
| Addressing | names, positions, selectors, placements | declarations by name, everything else by the handles the agent sees |
| To learn | a vocabulary of refactorings | ulang, plus `@put` |

The named operations are better at large changes across modules, a move of a declaration, an extraction with computed modes, `raises` carried upwards, so those come back as actions of their own (Operations, below).

## The command

`refactor` is the only command that edits sources by their meaning, as `format` is the only one that edits their layout: `check` only reports, also the edits that errors and lints propose. It is `ulang.sh refactor [--dry] [<script> | @<action> …]` (`refactor_request` of `ulang/ulang.u`), the script from a file or stdin or one action on the command line, run in one process, so that a unit is loaded and checked once, not per action; the edits of the lints are the action `@fix T [Rule …]`, all or those of the rules named, and the named operations below would be further actions of the same script, `@move` and so on.

What the design asks of every action, and what of it is built:

1. **Preconditions**: the design refuses an action unless the affected units load, check, and are formatted; built is only the refusal of a file that does not parse (`parsed` of `ulang/refactor/refactor.u`), since the reply lists the diagnostics that are new or gone, so errors before an edit do not hide what it changed.
2. **The edit**: the design edits the parsed tree; built is a splice of the text of whole declarations and nodes by the spans of the parsed file, which `format` then prints, since comments are not in the tree (Status).
3. **Postconditions**: every use outside the target resolves as before, modulo the renaming that the action declares, compared by the sequence of resolved names in the traversal order of each command, since positions move with the edit, which a rename keeps and a move maps by the old and new module, a difference reported as the use, the name before, and the name after; built only for `@rename` (`commit_rename` and `first_difference` of `ulang/refactor/refactor.u`), planned for the other actions.
4. **Output**: a script is one transaction, it writes all its files or none, and `--dry` reports without writing; the reply is plain text for agents, and `--json`, the edits and the affected sites as records, one per line, matching the JSON lines of the diagnostics, for the extension, is planned. Applying by default, where this file once proposed a dry run by default, follows the loop of the agent, which checks the state after the edit and goes on from its diagnostics, and `@undo` makes it reversible.
5. **Targets**: built are a file, a declaration `file.u:name[/n]` (`target_of`), and a handle `#n`; planned are `module.name/arity` for an overload (`prelude/set.size/1`), `module.T.f` for a field, `module.T.C` for a constructor, a position `file.u:L.C`, and a local as `file.u:proc/local`.

## Edit scripts

A script is a sequence of actions, each a line that starts with `@`, whose fragment is the lines up to the next, taken verbatim; the actions and their replies are those of Refactoring of `README.md`. Nothing else is needed for writing code: a case is added to a match by `@put into #h`, statements are wrapped in a `try` by `@put #h` of the `try` written around them, and a function is extracted by an `@put` of the function and an `@put` of the call in its place, and since every edited file is printed by `format`, a fragment never needs to be formatted by hand.

`@rename` is an action of its own since renaming is the one follow-up that the checker cannot drive: after a change of the text from `then` to `chain`, every use is merely unknown. It changes the declaration and every use at once, refuses what is no plain name and a name that the package declares already at the arity, with the reason, and undoes the script if any use resolves differently; the fields and constructors of a datatype, `json/query.u:Query.Pipe.then`, are item 4.

### Handles

`@show` puts the handles in a left margin, which `@put` strips again when code with handles is pasted back into a fragment. Each is kept in `out/refactor/handles.txt` of the root by its number, its file, the key of its declaration, its index among the nodes of the declaration, and the hash of the text of its node (`handle_line` of `ulang/refactor/refactor.u`), so it stays valid across formatting and edits elsewhere; the edits of a script move those of their declaration (`rebase` and `aligned`), and a handle whose node an edit from outside changed is `stale` when it is next used, one whose node an edit removed `gone` (`resolved`). Planned: `gone #9 (replaced by #31)` in the reply of the edit that removed the node.

### Follow-ups as diagnostics

After a change of a datatype, a signature, or a name, the reply lists every site that no longer checks, so the agent fixes them in the same script or the next, by `@put` at the handles that the reply gives them. Planned: the follow-ups that leave no decision are fixes in the sense of Refactor, fix, lint, optimize, below, and are applied and listed as applied: a new field becomes `_` in patterns, a removed field becomes `_` in patterns and is dropped from constructions, a removed formal is dropped from the calls whose argument for it has no effect, and the arguments of a call are put in the order of the modes after an `@put` of a signature whose formals moved between `in`, `out`, and `ref`, which `change-mode` of operation 3 does as a named operation. What needs a value or a decision is left to the agent as a diagnostic: a new field at a construction, a new case of a match, a new `in` formal at a call. Adding a title to links, which took that agent three failed text edits, is then an `@put` of the datatype with the field `title: Str` in `Link`, whose reply lists the patterns of `Link` as applied and the two constructions as `expects 3 fields, got 2` with their handles (item 2), and a script of an `@put` at each: two scripts and no search; with the defaults of fields (Changes to ulang, 4), one script without diagnostics. So `add-field`, `add-constructor`, `fill-cases`, and `add-formal` below are an `@put` and its follow-ups, and need no actions of their own unless the follow-ups become too many to fix by hand.

### Errors in fragments

A fragment that does not parse is refused at its own line and column, `fragment 1.3: …` and at its line in the script, `fragment 1.3, script 4.3: …` (`parse_message` and `with_script_lines` of `ulang/refactor/refactor.u`), with the hints of the habits of other languages, `hint_at` and `operator_hint` of `ulang/parse/parse.u` and `hint_of` of `ulang/check/message.u`. Planned: the corrected fragment in the reply, `corrected:  x := 2` after `an assignment is written x := e, without assign`, also for `return`, `break`, `+=`, `+` of strings, `[1, 2]`, modes out of order, a keyword as a name, and an `else if` without its own `end`, which `@retry` applies, the one action beyond the built ones, which exists only for them; the tool is the cheapest place for these hints, since it sees every piece of code an agent writes before it reaches a file.

## Queries first

Before any edit, an agent needs to find what to edit: the queries `resolve`, `refs`, `sig`, and `names`, and the record of resolutions behind them, which `@rename`, the handles of diagnostics, and the comparison of resolutions need too, are Diagnostics 3; `@outline` and `@refs` are built as actions of a script.

## Operations, by value for an agent

Named operations, actions beyond those built: rename is `@rename`; those that are an `@put` with its follow-ups (operations 2, 3 except `write-raises` and `function-to-procedure`, and 4 except `tuple-to-record` and `introduce-alias`) wait until the loop of `@put` and diagnostics proves too slow for them; the rest, moves, rewrites by pattern, `write-raises`, `function-to-procedure --transitive`, `extract-procedure`, and `to-case`, change many sites by an analysis that an agent gets wrong by hand, and are worth having as actions.

### 1. Rename

The most frequent refactoring, and the one where text tools fail by overloading, by strings, and by field punning. `@rename` does it for functions, procedures, pattern aliases, and constructors, only the selected overload, so renaming `set.size` leaves `map.size`, in export and implementation alike; the rest waits for the record of resolutions (item 4):

- **Declarations**: types, type aliases, lemmas, and tests.
- **Fields**: the declaration in every constructor that has it (one type across constructors), accessors `e.f`, updates `with f :=`, and the pattern variables that bind the field by its name, with their uses in the case, as the rule of style "patterns bind the names of the fields" requires; refused where the new name would hide a local (`HiddenLocal`) or clash in the case.
- **Locals, formals, type parameters**, with `HiddenLocal` as the guard, which also makes this the fix for that error.
- **Packages and modules**: move the directory or file, rewrite the `import` lines and the qualified names in docs; refused for names with `-` or keywords in the path.
- **References in the docs**: `` `old` of `file` `` in `CLAUDE.md`, `README.md`, `ideas/`, and the `//` docs of the sources, rewritten by the grammar of `checks/docs.u`, where `@rename` now only lists them, and reported separately so that an agent sees prose that may need rewording.

### 2. Expand `case _` and fill in missing cases

- `expand-wildcard file.u:L.C`: replace the `case _` of a match over a datatype by one case of the remaining constructors as alternatives, with the body of the wildcard, as `ideas/or-patterns.md` plans; the style rule asks for exactly this, and Code 1 lists the places left.
- `fill-cases file.u:L.C --body <expr>`: the missing cases that `NotExhaustive` reports, with a body given by the agent (for procedures commonly `raise Unexpected("…")`), as the fix that Diagnostics 2 wants offered with the error.
- With `add-constructor` (below), the listing of every match that the new constructor makes non-exhaustive, as one report rather than an error per run.

### 3. Change a signature with its calls

- `add-formal <target> <name>: <type> --arg <expr> [--at i]`: the formal in export and implementation, the argument at every call, with `<expr>` evaluated in the caller's scope, refused if it does not check there.
- `remove-formal`, refused if the formal is used in the body; `reorder-formals`.
- `change-mode <proc> <formal> in|out|ref`: the formal moves to its group, the arguments at each call too, since arguments go by mode in the order `in`, `out`, `ref`, which is the kind of reordering agents get wrong.
- `write-raises [<target>]`: write the inferred raises of private procedures, or the reported ones of `UndeclaredRaise`, into the signatures, export and implementation alike, and into procedure types of formals that `UntypedRaise` names; this turns a listing of errors of raises into one command.
- `function-to-procedure <fun>`: the function becomes a procedure with an `out` result, each call in a statement becomes a call of the procedure before it, into a fresh `var`; calls inside expressions of other functions are listed, since those functions must turn into procedures too (`--transitive` does so). This was the cost of removing `raise` from expressions.

### 4. Datatypes

- `add-field <T.C> <name>: <type> --default <expr> [--at i]`: the field, the argument at every construction, `_` in every pattern of `C`, and nothing for accessors; with the planned positional patterns this is the case where a text edit misses a site.
- `remove-field`, refused if a pattern binds it or an accessor reads it; `reorder-fields`.
- `add-constructor <T> <decl>`: the constructor, plus the report of the matches it opens (operation 2).
- `tuple-to-record <type position> <Name>(<field>: …)`: replace a tuple type in a signature or field by a new datatype, rewriting constructions `(a, b)` and patterns `(x, y)` of values of that type, found by type, not by syntax; this was `8e97aed`.
- `introduce-alias <pattern> as <Name>(formals)`: declare a pattern alias and replace every pattern, and every construction where it builds, that is an instance of it, as `2ab6b2f` did for `VInt`, `ENil`, and the like; a guard `if constr == cons_name()` is listed as a candidate.

### 5. Moves

- `move <decl> <module>`: the declaration with its doc and tests, the signature into the export section of the target if it was exported or is used outside, imports added to the users that now need the target's package and removed where unused; refused with the list of the private names it uses that the target does not see (`--with-deps` moves them along, if nothing else uses them), and refused if the imports would form a cycle of packages or break the layers (`ulang/syntax` before `ulang/format` before `ulang/parse` …). Moves within a package need no imports, since members see each other, and the visibility of a declaration is `@export` and `@unexport`. This is Code 4.
- `move-test <test> <file>`: into the file of its feature, refused on a name clash there; helpers it needs move or are listed.

### 6. Rewrites by pattern

A structural search and replace on the parsed tree, with metavariables that may carry types, for the one-off migrations that no named operation covers:

```
ulang.sh refactor rewrite 'List(for $x in List(for $e in $m)) $rest' '…' --where '$m: Map[_, _]'
ulang.sh refactor rewrite '$xs := $xs ++ List($v)' '$xs := $xs ++ List($v)' --where '$xs: Buffer[_]'
```

Patterns are written in ulang syntax and parsed by the parser of their context (expression, statement, pattern, type), metavariables `$x` match a part, `$x...` a list of arguments or statements, and `--where` constrains their types from the checked program. The rewrites of Diagnostics 5 (`lint.md`) are then a library of such rules with tests, and an agent writes its own for a migration instead of a regular expression. A rewrite whose result does not check, or changes another use's resolution, is refused per site, and the others are applied.

### 7. Statements and locals

- `extract-procedure file.u:L.C-L.C <name>`: the statements become a procedure whose `in`, `out`, and `ref` formals are computed from what the block reads, assigns, and must define on every path, and whose `raises` from what it raises; Diagnostics 4 rejects extraction because text edits do it well, which holds for functions but not here, where the modes and definite assignment are the analysis an agent gets wrong; worth reconsidering for procedures only.
- `inline <fun>` at one call or all: for the opposite direction, where the call becomes a `let` of the formals, and refused for functions without parameters, which are computed once.

### 8. Tests and cases

- `to-case <file.u> --kind parse|types|…`: turn a failing program into a case of the file of its kind in `ulang/test/errors/`, the expected exception taken from a run of it, as the rule demands ("take the expected exception from a run, never guess it"), with positions counted in the text of the case; `15b55d4` did this 211 times.
- `rename-test`, `move-test` as above.

## Refactor, fix, lint, optimize

Not every change of code belongs to a command that an agent invokes with a target. Four kinds, by who decides and where the change lands:

| Kind | Decided by | Scope | Lands in | Run |
|---|---|---|---|---|
| refactoring | the agent: a name, a place, a default | any, across modules | the sources | an action of `refactor`, on request |
| fix | an error, with one obvious edit | the site of the error | the sources | listed with the error by `check`, applied by `@fix`, planned |
| canonicalization | the code alone: a normal form exists | local, one part | the sources | reported by `check --lint`, applied by `@fix` |
| optimization | the cost of evaluation | local or whole program | the C only | `compile`, always |

The tests that sort a change:

- **Does it need a choice that the code does not determine** (a new name, a target module, a default argument, the body of a new case)? Then it is a refactoring.
- **Is it the one edit that removes an error?** Then it is a fix: `write-raises` for `UndeclaredRaise` and `UntypedRaise`, `_` for an unused pattern variable once that is an error, the removal of an unused import (`UnusedImport`), the removal of a handler that is `NeverRaised`, a qualified name for an `Ambiguous` use where only one candidate checks in its context, and the hints of Diagnostics 2 for the habits of other languages (`x += e` to `x := x + e`, `[1, 2]` to `List(1, 2)`, `s + t` of strings to `s ++ t`).
- **Is it determined by the code, meaning-preserving, and better by a cost that every reader shares?** Then it is a canonicalization, for lint. To be applied without review it must also be terminating (the cost strictly decreases), confluent (the order of applying rules does not matter), and idempotent, as `format` is; the e-graph of `lint.md` gives the first two by construction, since extraction picks one cheapest representative per class.
- **Is it only faster, and less clear to read?** Then it is an optimization and never edits the sources: fusion of comprehensions, which `ulang/compile/compile.u` already does by nested loops, unrolling, constant folding, a fold that `size(List(for …))` counts instead of building the list, sharing of repeated pure subexpressions, and the extraction by the cost of evaluation (`lint.md`, step 6 of its order). Rewriting the source for speed would undo the canonical form that lint establishes.

So of the operations above, two are no operations of their own, but edits that `@fix` applies: moving locals into the innermost body that uses them is `LocalTooHigh` of `ulang/lint/lint.u`, built, since the style rule fixes the place, so no choice is left, and `write-raises` is a fix of errors, planned. `expand-wildcard` stays a refactoring, which a lint of a `case _` (Diagnostics 5) offers as its fix where the match names two or more constructors besides it (`ideas/or-patterns.md`).

### Canonical forms missing from `lint.u` and `lint.md`

Candidates beyond the rules of `ideas/lint.md` and the walks that Diagnostics 5 holds open, a `match` of one irrefutable case, an `if contains(k, m)` before a `get(k, m)`, and appends in loops that suggest a `Buffer`, each determined by the code and each a rewrite with a test before and after:

- **Booleans and conditionals**: `match c case true => a case false => b end` to `if c then a else b`; `if not c then a else b` to `if c then b else a`; `if c then skip else s end` to `if not c then s end`; `if a then b else false` to `a and b`, `if a then true else b` to `a or b`; `not exists(xs, p)` and `forall(xs, not p)` to one of them.
- **Empty containers and identities**: `size(xs) == 0` to `xs == List()`, also of sets, maps, and strings; `xs ++ List()`, `List() ++ xs`, `s ++ ""`; `e with f := e.f`; `let x := e in x` to `e`.
- **Lambdas**: `lambda x => f(x)` to `f` where `f` is one declaration, not an overload that the lambda's type resolves, which the comparison of resolutions decides; a `foldr` or `foldl` whose step only conses or adds to a set or a map to the comprehension.
- **Maps**: `for (k, v) in List(for x in m)` to `for k => v in m`, the rule that `CLAUDE.md`, Style, states from a mistake (`field_table_decls`), where `CopiedContainer` of `ulang/lint/lint.u` gives `for (k, v) in m`; a `match` on `get(k, m)` that spells out `value_or` is `Folded`.
- **Statements**: a `try` whose only handler raises what it caught, unchanged, to its body; a local whose initial value every path overwrites before reading it, to a declaration without a value.
- **Names by the rules of style**: a pattern variable that binds a field under another name, where the field's name is free, to the field's name (Style: "patterns bind the names of the fields"); numbered names (`NumberedName`) are reported but not renamed, since the new name is a choice, which `@rename` then carries out.
- **Order of declarations**: the definitions of `implementation` in the order of their signatures in `export`, which keeps diffs of the two sections parallel; only if the order is not used as documentation, so opt-in.

### Smells that are reported, not rewritten

Their fix is a choice of data structure or algorithm, so lint names the site and the cost, and the agent decides; appends in loops, also of strings, are those of Diagnostics 5:

- `contains(x, xs)` of a list inside a loop or comprehension over another container, quadratic, where a `Set` would do;
- `at(i, xs)` or `get` of an association list in a loop;
- `foldr` over a list that grows with the input, which recurses as deep as the list (the stack is unlimited, but the depth is still memory);
- the same `get(k, m)` repeated in one body, which is a `let` or a `match` once.

### What connects them

- **One frame of edits, two owners.** The rule and its edit stay together, the application of edits does not: a rule of `ulang/lint/lint.u` finds a place and the edit to make there in one match, as an `Edit` of `ulang/syntax/syntax.u` in its `Warning`, so that the two cannot drift apart; `check --lint` prints the warning, and `@fix` applies the edit (`fix` and `local_splices` of `ulang/refactor/refactor.u`), so lint never writes files by a path of its own and has no `--apply`, and the test `fixes` of `ulang/refactor/test/scripts.u` keeps one case of each rule. Planned: the hints of errors and the operations of agents make the same kinds of edits, and `@fix` takes them all through the comparison of resolutions and the replay under concurrent edits, one safety check for every source of edits.
- **One matcher.** The rewrites by pattern of operation 6 are the rules of lint written in ulang syntax: the library of canonical forms is a set of rules shipped with the tool, and a project adds its own for a migration, which is a lint rule that runs once.
- **Project policy in `project.u`.** What the rules would hard-code, which rules are on and the threshold of R4 of `ideas/lint.md`, are fields of `Project` of `ulang/prelude/os.u`, as the directories that run only compiled already are.
- **An enforced tier.** Rules that are terminating, confluent, and uncontroversial can be enforced as `format` is: `check` fails on a part that is not canonical, and `make reformat` applies them by `format` and `@fix` of those rules, so agents never chase them as warnings. The rest stays opt-in, which answers open question 5 of `ideas/lint.md` per rule rather than for all.

## Output for agents

- **A script applies, `--dry` only reports** (The command, 4): the reply shows what changed and the diagnostics that changed, not the diff, which costs more context than the agent needs to go on.
- **Each refusal names the site and the reason** in the terms of the errors (`HiddenLocal at 12.5`, `uses private parse_type`), and a suggestion (`--with-deps`, a different name), since an agent learns the tool from its messages.
- **Batch by script**, built: actions applied in order on the text of the previous one, one check of each file before the writes and one after, and a note per action, so that a migration of 20 renames is one run instead of twenty.
- **Exit status**: 0 applied, 1 refused, 2 usage, as for the other commands, and 3 if a test of `@check test` fails; 1 also while errors remain.
- **The VS Code extension** gets rename (F2) and the code actions of operations 2 and 3 from the same commands with `--json`, planned.

## Concurrent edits

Several sessions edit the same files at once, also `CLAUDE.md` and `ideas/`, and each commits only its own hunks (working rules of `CLAUDE.md`). A refactoring reads many files, checks for a second, and writes many files, so another session can write in between, and a rename that missed the uses another session just added breaks its build. What helps, from cheap to thorough, planned but for the journal of 6:

1. **Operations are intent, not text.** A refactoring is stored as what it does, `rename prelude/set.size/1 cardinality`, addressed by names, not by positions, so it can be run again on any later version of the files. That is the main lever: conflicts are resolved by replaying the operation, not by merging its diff. Targets by position, `file.u:L.C`, are turned into names when the operation is planned, and a script keeps the names.
2. **Optimistic writes.** A script records the hash of each file it read; just before writing, it reads them again, and if any changed, it replays the operation on the new text (preconditions, edit, check, comparison) and tries again, a few times, then gives up with the files that keep changing. Each file is written to a temporary file and renamed, so no reader sees half a file, and the check–compare–write window is guarded by a lock, `out/refactor.lock` by `flock`, held only for the comparison and the writes, never for the check, which the replay repeats instead. Other tools that write sources, `format` and the editor, take the same lock or the same hashes.
3. **Uses added after the rename.** A replay finds them, since `refs` is by resolution. For sessions that are still writing code against the old name, `rename --keep-old` leaves the old declaration as a forwarder, `function size[A](s: Set[A]): Int := cardinality(s)`, marked by a doc, which lint reports and `refactor remove-forwarders` removes later by inlining; the transition then needs no coordination.
4. **Uncommitted edits of others that do not check.** The precondition "the units check" then fails for reasons that are not the agent's. `--base HEAD` runs the operation on the committed version, `git show HEAD:file`, in memory, and replays it on the working tree, where only the files the operation touches need to parse, and the result per file is either applied or reported as a conflict; the hunks of the operation are its own, which matches the rule of committing only one's own hunks, and `--patch out.diff` writes them for `git apply --cached`.
5. **Merging at the level of declarations.** Since `format` is canonical, two versions of a file differ by whole commands far more often than by lines within them, so a three-way merge of the parsed trees by command (base, ours, theirs; a command changed on one side only is taken, on both sides is a conflict, compared by `strip_cmd`) resolves what a line merge reports as a conflict only because a neighbouring line moved; `ulang.sh merge base ours theirs` as a git merge driver for `*.u`, also usable by sessions that do not refactor. It needs the diff of declarations of `lint.md` (P1).
6. **A journal.** Built: each script that writes is `out/refactor/journal/N.txt` of the root, with the text of each of its files before and after it (`journal_text` of `ulang/refactor/refactor.u`), and `@undo n` restores them, refused if a file changed after the script. Planned: the session and time of each script, the replay of the inverse where other edits followed, and `refactor log`, which tells a session which changes in the working tree are its own when it commits, and which operations of other sessions touched the files it is about to edit.
7. **Editor buffers.** Unsaved edits in VS Code are versions too: the extension passes them as overlays, `--overlay file.u=<path of the buffer contents>`, so that a rename from the editor sees them, and it reloads the files the operation wrote.

Out of scope: operational transformation or CRDTs at the level of characters, which solve live co-editing of one buffer, not the edits of agents, which are seconds to minutes apart and best rebased as operations.

## Changes to ulang

Needed by the tool:

1. **The record of resolutions** of Diagnostics 3, which declaration each name, field, and constructor resolves to, for `@rename`, the handles of diagnostics, and the comparison of resolutions.
2. **A single node formats to the same text as in its file**, and formatting stays canonical over the error nodes, so `@show`, `@patch`, and the code of diagnostics always match the file.

So that additions break nothing:

3. **Patterns of fields by name**, `case Link(url := u)`, which bind only the fields named, the others implicitly `_`, and do not change when a field is added, removed, or reordered; positional patterns stay.
4. **Defaults of fields**, `case Link(body: List[Inline], url: Str, title: Str := "")`, where a construction may leave out trailing fields with defaults, unambiguous since constructors are not overloaded by arity; not for the formals of functions and procedures, where they would clash with overloading by arity.

With 3 and 4, adding a field, the most common change of data, is one `@put` of the datatype without follow-ups, and `add-field` of operation 4 is not needed.

For drafting:

5. **A hole, `todo`**, an expression of any type and a statement, which raises `Todo` at runtime, is an open goal for the prover, and whose remaining uses `check --test` lists, so a draft checks before every case is written, and a new constructor is stubbed in each match by `@put into #h` of `case Scroll(_) => todo`.
6. **An optional inline export**, `export function f(…): T := …` beside the sections, so that a fragment is the whole truth of a declaration; optional, since `@put` and `@export` keep the sections in step anyway.

What can stay strict: with the tool, locals in the innermost body that uses them, the canonical format, the signatures that `implementation` repeats, and the order of modes cost an agent nothing, so they stay, and docs, which are not in the tree, move with their declaration, since an edit splices the lines of the declaration with the comments right above it, so no structural edit loses them.

## Order of implementation

Done: the edit scripts with their transaction, replies, handles, and the journal of `@undo`, but for the lock and the replay of Concurrent edits; `@fix` of the edits that the lints carry; `@outline` and `@refs`; of the operations, the rename of declarations, several per script, and the writing of modules, imports, and exports. So the order is now:

1. The small items 3, 6, 7, 8, and 9.
2. `--dry` with its check, Open items 1, which needs the overlays of `load_file`.
3. `@example` and the handles of the lines of a spec, Open items 5.
4. `resolve`, `sig`, `names`: the other queries of Diagnostics 3, by position on the checked program.
5. The record of resolutions, Changes to ulang 1, then `@rename` and `@refs` of fields, types, and locals (Open items 4); then of modules with docs.
6. The mechanical follow-ups, the hints of fragments with `@retry`, `expand-wildcard`, and the fixes of errors in `@fix`, `write-raises` and the others: small, and each the fix of an existing error; with the messages of item 2 from the checker, Diagnostics 6.
7. The changes of ulang 3 to 6, at any time, each making step 6 smaller.
8. Signature changes and datatype changes beyond `@put`, if the follow-ups prove too many: `tuple-to-record`, `introduce-alias`, `function-to-procedure`.
9. `move`.
10. Rewrites by pattern, shared with lint.
11. `extract-procedure`, `to-case`, as needs show.

Each operation has tests in `ulang/refactor/test/`, scripts on files before and after, which run only compiled, and its refusals there too.
