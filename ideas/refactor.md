# Refactor: scripted editing of ulang files by `ulang.sh`

Suggestions. It extends Diagnostics 3 and 4 (resolution of names, refactorings) with the operations that the history of this repository shows agents doing by hand, and with what an agent needs from the command line to drive them. It takes in the second version of `edit-tool.md`, now removed, the design of an agent that wrote about 4,100 lines of ulang in another project on 12–13 November 2026: the interface of `refactor` is a script of seven actions that writes code by declaration and reports follow-ups as diagnostics with handles (Edit scripts, below), and the named operations of this file are actions that come later, where that loop proves too slow. When something here is decided, it goes into those items, and this file is deleted.

## Why ulang is unusually suited to it

- **`format` is canonical and `check` enforces it**, so a tool that changes the parsed tree and prints it by `format_program` produces a diff of exactly the changed parts; no layout is lost, and docs survive since they are syntax.
- **The checked program keeps every position** (`At` since `1c2db95`) and resolves every name to its module, so the qualified name and type of any use are found by its position; overloading, which defeats `grep` (`size` in a file that imports `set` and `map`), is no obstacle once the checker is asked.
- **The parser desugars nothing**, so the tree that is rewritten is the source as written.
- **The checks are strong** (exhaustiveness, `HiddenLocal`, definite assignment, `raises`), so "check again after the edit" catches most broken refactorings, and the remaining risk is a use that silently resolves differently, which the tool can rule out by comparison (below).
- **A declaration names itself**: within a module, overloading is only by arity, so a fragment `function emphasis(…) := …` says which declaration it replaces by its name and arity, and an edit of whole declarations needs no address.
- **`@` occurs nowhere in ulang**, so a script can delimit fragments by lines starting with `@` and take them verbatim, without escaping.
- **Docs reference code in a checked grammar** (`` `name` of `file` ``, `checks/docs.u`), so moves and renames can update `CLAUDE.md`, `README.md`, and `ideas/` too, which Code 4 demands and agents forget.

## What the history shows done by hand

Each of these was a commit of many mechanical edits, several with regular expressions, one of which went wrong (unclear.md: `(` and `)` as boundaries skipped every name in an argument list):

- renames of fields by role, with patterns rebinding the new names (`2543f3f`), of constructors (`Failure` to `RuntimeError`, `GFor` to `GVal`, `Elem` to `Yield`), of a package (`self` to `ulang`, `f7dc044`), of functions (`alias_of` to `type_alias_of`), of locals that hid others (`32f41e2`);
- positional tuples to records with selectors (`8e97aed`);
- pattern aliases replacing spelled-out shapes and guards (`2ab6b2f`);
- moves of declarations between modules and of tests between files (`26c9c42`, `64a4092`), and the split of `ulang/` into a package per phase (`ff02be2`);
- functions turned into procedures when they had to raise (`8084d78`: `as_type`, `type_apply`), and `raises` written into the exported signatures (`9335b69`);
- 211 failing files turned into cases of `ulang/test/errors/` (`15b55d4`);
- the expansion of `case _` into named constructors, still open (Code 1);
- exports added because another member needed a private helper (unclear.md: `covers_all`, `strip_yield`).

## How an agent edits

From that session:

1. It writes whole declarations as text, and replaces a whole function more often than it changes a line in it.
2. It batches several changes, then checks once.
3. Its failed edits were text matches against code that `format` had reshaped, or edits by line number after earlier edits had moved the lines.
4. The expensive part is the follow-ups: after a change of a datatype or a signature, finding every site that has to change too.
5. It already uses the diagnostics as a list of work: change, check, fix what is reported, until nothing is.

So the tool makes this loop safe and cheap before it adds a vocabulary of refactorings, which is much to learn for an agent with almost no ulang in its training, and against the principle of one way of each thing. The first version of that design had about 40 actions, one per refactoring, as the operations below are; the second has seven:

| | Named operations | Edit script |
|---|---|---|
| Actions | about 40, one per refactoring | 7, plus `@retry` |
| Code | in arguments, by operation | in fragments, by declaration, which names itself |
| Follow-ups | each operation updates its sites | the checker reports them, with handles; mechanical ones applied |
| Addressing | names, positions, selectors, placements | declarations by name, everything else by the handles the agent sees |
| To learn | a vocabulary of refactorings | ulang, plus `@put` |

The named operations are better at large changes across modules, a move of a declaration, an extraction with computed modes, `raises` carried upwards, so those come back as actions of their own (Operations, below).

## The command

`refactor` is the only command that edits sources by their meaning, as `format` is the only one that edits their layout: `check` and `lint` only report, also the edits that errors and lints propose, and `format` always rewrites, since `check`, `lint`, and `test` report the files that differ (`format --check` and `check --lint` are gone, `lint` is a command of its own as of `walks`).

One command, `ulang.sh refactor --script <file> [--dry] [--json]`, or the script on stdin, whose actions are those of Edit scripts, below, run in one process, so that a unit is loaded and checked once, not per action (checking `ulang/ulang.u` takes about a second); a single action may be given on the command line, `ulang.sh refactor @rename …`, as a script of one line. The operation `fix [<path> …] [--rule <Code> …]`, the edits that the errors and lints of the files propose, all or those of the rules named, and the named operations below are further actions of the same script, `@fix`, `@move`, and so on.

Common to all operations:

1. **Preconditions**: the affected units load, check, and are formatted, else the operation is refused with the errors; nothing half-applied.
2. **Edit on the parsed tree**, at positions found in the checked program, printed by `format_program`.
3. **Postconditions**: the edited units check, and every use outside the target resolves as before, modulo the renaming the operation declares. Positions move with the edit, so the comparison is by the sequence of resolved names in the traversal order of each command, which a rename keeps and a move maps by the old and new module; a difference is reported as the use, the name before, and the name after.
4. **Output**: a script is one transaction, it writes all its files or none, and `--dry` reports without writing; the reply is plain text for agents (Replies, below), `--json` gives the edits and the affected sites as records, one per line, matching the planned JSON of Diagnostics 1, for the extension. Applying by default, where this file proposed a dry run by default, follows the loop of the agent, which checks the state after the edit and goes on from its diagnostics; `@undo` and the journal of Concurrent edits make it reversible.
5. **Targets** extend the existing `file.u:name`: `file.u:name/n` for a declaration of arity `n`, `module.name/arity` for an overload (`prelude/set.size/1`), `module.T.f` for a field, `module.T.C` for a constructor, `file.u:L.C` for the use at a position, a local as `file.u:proc/local`, and a handle `#n` for a node that a reply showed.

## Edit scripts

A script is a sequence of actions. An action starts with a line beginning with `@`, and the lines up to the next `@` line are its fragment, taken verbatim.

| Action | Fragment | What it does |
|---|---|---|
| `@show T` | none | the canonical text of `T` with handles; `T` is a file, a declaration `file.u:name/n`, or a handle |
| `@put [F] [where]` | declarations, or statements, cases, an expression | writes code, below |
| `@drop T` | none | removes a declaration or a node |
| `@rename T new` | none | renames a declaration, field, constructor, formal, local, or module everywhere, operation 1 |
| `@patch T` | lines `old => new` | text substitution within the canonical text of `T` only, each `old` occurring exactly once |
| `@check [T]` | none | diagnostics, tests (`@check test`), lemmas (`@check prove`); implied at the end of every script |
| `@undo [n]` | none | undoes the last script, or script `n` of the journal |

### `@put`

- **Declarations, without placement**: each declaration of the fragment replaces the one of the same name and arity in the file `F`, or is added at the end of its section if there is none; `@put F after file.u:name/n` places new ones after a given declaration. An `export` in front of a definition (Changes to ulang, 6) puts its signature into the `export` section; without that change, the tool keeps the signature in `export` and the definition in `implementation` in step itself, also the contracts and examples, which stand on the signature of an exported name.
- **Code into a declaration**: `@put #h` replaces the node `#h` by the fragment, which must be of the same kind, a statement, a case, or an expression; `@put after #h`, `@put before #h`, and `@put into #h`, as its last statement or case, insert new nodes.
- **Hoisting**: a `local x := e` among the statements of a fragment becomes a declaration of `x` on the `local` line of the innermost body that uses it and `assign x := e` in its place, so the rule of style on locals costs nothing; this is the edit of `LocalTooHigh`.

Nothing else is needed for writing code: a case is added to a match by `@put into #h`, statements are wrapped in a `try` by `@put #h` of the `try` written around them, and a function is extracted by an `@put` of the function and an `@put` of the call in its place. Every fragment goes through the preconditions and postconditions above and is printed by `format_program`, so a fragment never needs to be formatted by hand.

`@rename` is an action of its own since renaming is the one follow-up that the checker cannot drive: after a change of the text from `then` to `chain`, every use is merely unknown. It changes the declaration and every use at once and refuses keywords, the names of forms, and names that would clash, with the reason; for a datatype it also renames fields and constructors (`json/query.u:Query.Pipe.then`).

### Replies

Short enough to read in full every time:

```
ok | ok, 3 diagnostics | rejected | dry
<one block per action: what changed>
<diagnostics: only what changed, with handles>
```

Handles stand in a left margin, which the tool strips again when code with handles is pasted back into a fragment:

```
@show json/query.u:term/2
   procedure term(out q: Query, ref cs: List[Char]) raises QueryError :=
#7   local w: Str
#8   match cs
#9     case List('.'; _) =>
#10      call path(out q, ref cs)
#11    case _ =>
#12      call word(out w, ref cs)
         …
```

Handles appear only where the agent sees code anyway, in `@show` and in diagnostics, and stay valid across formatting and edits elsewhere; when their node goes away, the reply says `gone #9 (replaced by #31)`, and an edit from outside the tool, noticed by the hashes of the files (Concurrent edits, 2), makes the handles of the changed declarations `stale`, which the next reply names. They are nodes of the parsed tree, kept by the `At` of each part, not positions, which edits move.

### Follow-ups as diagnostics

After a change of a datatype, a signature, or a name, the reply lists every site that no longer checks, with its handle and its code, so the agent fixes them by `@put` on the handles, in the same script or the next. The follow-ups that leave no decision are fixes in the sense of Refactor, fix, lint, optimize, below, and are applied and listed as applied:

- a removed field becomes `_` in patterns and is dropped from constructions;
- a removed formal is dropped from the calls whose argument for it has no effect;
- the arguments of a call are put in the order of the modes after an `@put` of a signature whose formals moved between `in`, `out`, and `ref`, which `change-mode` of operation 3 does as a named operation.

What needs a value or a decision is left to the agent as a diagnostic: a new field at a construction, a new case of a match, a new `in` formal at a call. Adding a title to links, which took that agent three failed text edits:

```
@put md/inline.u
data Inline :=
  case Text(s: Str)
  …
  case Link(body: List[Inline], url: Str, title: Str)
  …
```

```
ok, 2 diagnostics
md/inline.u: data Inline replaced
applied: #53 render_inline/1  case Link(body, url, _) =>
applied: #54 plain_inline/1   case Link(body, _, _) =>
diagnostics: +2
  + #51 link/1      Link(inlines(text, '['), url)    expects 3 fields, got 2
  + #52 autolink/1  Link(List(Text(url)), url)       expects 3 fields, got 2
```

```
@put #51
Link(inlines(text, '['), url, title)
@put #52
Link(List(Text(url)), url, "")
```

Two scripts and no search; with the defaults of fields (Changes to ulang, 4), one script without diagnostics. So `add-field`, `add-constructor`, `fill-cases`, and `add-formal` below are an `@put` and its follow-ups, and need no actions of their own unless the follow-ups become too many to fix by hand.

### Errors in fragments

A fragment that does not parse is reported at its own line and column, with the hints of the habits of other languages, `hint_at` and `operator_hint` of `ulang/parse/parse.u` and `hint_of` of `ulang/check/message.u`, and the corrected fragment:

```
rejected
@put #12, fragment 1.3: an assignment starts with assign
  corrected:  assign x := 2
  @retry applies the corrections
```

So also for a call without `call`, `return`, `break`, `+` of strings, `[1, 2]`, modes out of order, a keyword as a name, a `local` after statements, which the tool hoists anyway, and an `else if` without its own `end`. The tool is the cheapest place for these hints, since it sees every piece of code an agent writes before it reaches a file; `@retry` is the one action beyond the seven, and exists only for them.

## Queries first

Before any edit, an agent needs to find what to edit; these are Diagnostics 3, and every refactoring is built on them:

- `resolve file.u:L.C`: qualified name, type, and declaration of the use there.
- `refs <target>`: all uses in the project, by resolution, not by text, with positions and the enclosing declaration, also of fields (accessors, updates, constructions, patterns).
- `sig <module>`: the exported signatures without bodies, the cheap way to learn a package (Writing ulang tells agents to read the `export` sections).
- `outline <file>`: declarations with kind, position, exported or private, and line count, so that an agent reads a 4000-line `check.u` by parts.
- `names <word>`: every use of a word as any name, declaration, field, local, formal, or pattern variable, in the project and the library, which the working rule before making a word a keyword asks for (unclear.md: `raises` as a field name broke the seed).

`@show` of a file is the text with handles, `outline` the overview without bodies; the queries are actions of the script too, `@resolve`, `@refs`, `@sig`, `@outline`, `@names`, which only report. The record of resolutions behind `resolve` and `refs` is also what `@rename` and the handles of diagnostics need (Changes to ulang, 1).

## Operations, by value for an agent

Named operations, actions beyond the seven: rename is `@rename`; those that are an `@put` with its follow-ups (operations 2, 3 except `write-raises` and `function-to-procedure`, and 4 except `tuple-to-record` and `introduce-alias`) wait until the loop of `@put` and diagnostics proves too slow for them; the rest, moves, rewrites by pattern, `write-raises`, `function-to-procedure --transitive`, `extract-procedure`, and `to-case`, change many sites by an analysis that an agent gets wrong by hand, and are worth having as actions.

### 1. Rename

The most frequent refactoring, and the one where text tools fail by overloading, by strings, and by field punning.

- **Declarations**: function, procedure, type, alias, constructor, pattern alias, lemma, test; only the selected overload, so renaming `set.size` leaves `map.size`. In export and implementation alike.
- **Fields**: the declaration in every constructor that has it (one type across constructors), accessors `e.f`, updates `with f :=`, and the pattern variables that bind the field by its name, with their uses in the case, as the rule of style "patterns bind the names of the fields" requires; refused where the new name would hide a local (`HiddenLocal`) or clash in the case.
- **Locals, formals, type parameters**, with `HiddenLocal` as the guard, which also makes this the fix for that error.
- **Packages and modules**: move the directory or file, rewrite the `import` lines and the qualified names in docs; refused for names with `-` or keywords in the path.
- **References in the docs**: `` `old` of `file` `` in `CLAUDE.md`, `README.md`, `ideas/`, and the `//` docs of the sources, by the grammar of `checks/docs.u`, reported separately so that an agent sees prose that may need rewording.

### 2. Expand `case _` and fill in missing cases

- `expand-wildcard file.u:L.C`: replace the `case _` of a match over a datatype by one case per remaining constructor, each with the body of the wildcard and its variables as `_`; the style rule asks for exactly this over `Cmd`, `Expr`, and `Stmt`, and Code 1 lists the places.
- `fill-cases file.u:L.C --body <expr>`: the missing cases that `NotExhaustive` reports, with a body given by the agent (for procedures commonly `raise Unexpected("…")`), as the fix that Diagnostics 2 wants offered with the error.
- With `add-constructor` (below), the listing of every match that the new constructor makes non-exhaustive, as one report rather than an error per run.

### 3. Change a signature with its calls

- `add-formal <target> <name>: <type> --arg <expr> [--at i]`: the formal in export and implementation, the argument at every call, with `<expr>` evaluated in the caller's scope, refused if it does not check there.
- `remove-formal`, refused if the formal is used in the body; `reorder-formals`.
- `change-mode <proc> <formal> in|out|ref`: the formal moves to its group, the arguments at each `call` too, since arguments go by mode in the order `in`, `out`, `ref`, which is the kind of reordering agents get wrong.
- `write-raises [<target>]`: write the inferred raises of private procedures, or the reported ones of `UndeclaredRaise`, into the signatures, export and implementation alike, and into procedure types of formals that `UntypedRaise` names; this turns the error listing of Checker 1 into one command.
- `function-to-procedure <fun>`: the function becomes a procedure with an `out` result, each call in a statement becomes a `call` before it into a fresh local; calls inside expressions of other functions are listed, since those functions must turn into procedures too (`--transitive` does so). This was the cost of removing `raise` from expressions.

### 4. Datatypes

- `add-field <T.C> <name>: <type> --default <expr> [--at i]`: the field, the argument at every construction, `_` in every pattern of `C`, and nothing for accessors; with the planned positional patterns this is the case where a text edit misses a site.
- `remove-field`, refused if a pattern binds it or an accessor reads it; `reorder-fields`.
- `add-constructor <T> <decl>`: the constructor, plus the report of the matches it opens (operation 2).
- `tuple-to-record <type position> <Name>(<field>: …)`: replace a tuple type in a signature or field by a new datatype, rewriting constructions `(a, b)` and patterns `(x, y)` of values of that type, found by type, not by syntax; this was `8e97aed`.
- `introduce-alias <pattern> as <Name>(formals)`: declare a pattern alias and replace every pattern, and every construction where it builds, that is an instance of it, as `2ab6b2f` did for `VInt`, `ENil`, and the like; a guard `if constr == cons_name()` is listed as a candidate.

### 5. Moves and visibility

- `move <decl> <module>`: the declaration with its doc and tests, the signature into the export section of the target if it was exported or is used outside, imports added to the users that now need the target's package and removed where unused; refused with the list of the private names it uses that the target does not see (`--with-deps` moves them along, if nothing else uses them), and refused if the imports would form a cycle of packages or break the layers (`ulang/syntax` before `ulang/format` before `ulang/parse` …). Moves within a package need no imports, since members see each other. This is Code 4.
- `export <decl>` and `unexport <decl>`: the signature added to or removed from the export section, `unexport` refused while another module uses it; the fix for unclear.md's `covers_all` and `strip_yield`.
- `move-test <test> <file>`: into the file of its feature, refused on a name clash there; helpers it needs move or are listed.

### 6. Rewrites by pattern

A structural search and replace on the parsed tree, with metavariables that may carry types, for the one-off migrations that no named operation covers:

```
ulang.sh refactor rewrite 'List(for $x in List(for $e in $m)) $rest' '…' --where '$m: Map[_, _]'
ulang.sh refactor rewrite 'assign $xs := $xs ++ List($v)' 'assign $xs := $xs ++ List($v)' --where '$xs: Buffer[_]'
```

Patterns are written in ulang syntax and parsed by the parser of their context (expression, statement, pattern, type), metavariables `$x` match a part, `$x...` a list of arguments or statements, and `--where` constrains their types from the checked program. The rewrites of Diagnostics 5 (`lint.md`) are then a library of such rules with tests, and an agent writes its own for a migration instead of a regular expression. A rewrite whose result does not check, or changes another use's resolution, is refused per site, and the others are applied.

### 7. Statements and locals

- `hoist-locals <proc>` per the style rule that each `local` stands at the start of the innermost body that uses it, in each case that uses it: mechanical, checkable, and a frequent review comment.
- `extract-procedure file.u:L.C-L.C <name>`: the statements become a procedure whose `in`, `out`, and `ref` formals are computed from what the block reads, assigns, and must define on every path, and whose `raises` from what it raises; Diagnostics 4 rejects extraction because text edits do it well, which holds for functions but not here, where the modes and definite assignment are the analysis an agent gets wrong; worth reconsidering for procedures only.
- `inline <fun>` at one call or all: for the opposite direction, where the call becomes a `let` of the formals, and refused for functions without parameters, which are computed once.

### 8. Tests and cases

- `to-case <file.u> --kind parse|types|…`: turn a failing program into a case of the file of its kind in `ulang/test/errors/`, the expected exception taken from a run of it, as the rule demands ("take the expected exception from a run, never guess it"), with positions counted in the text of the case; `15b55d4` did this 211 times.
- `rename-test`, `move-test` as above.

## Refactor, fix, lint, optimize

Not every change of code belongs to a command that an agent invokes with a target. Four kinds, by who decides and where the change lands:

| Kind | Decided by | Scope | Lands in | Run |
|---|---|---|---|---|
| refactoring | the agent: a name, a place, a default | any, across modules | the sources | `refactor <op>`, on request |
| fix | an error, with one obvious edit | the site of the error | the sources | listed with the error by `check`, applied by `refactor fix` |
| canonicalization | the code alone: a normal form exists | local, one part | the sources | reported by `lint`, applied by `refactor fix` |
| optimization | the cost of evaluation | local or whole program | the C only | `compile`, always |

The tests that sort a change:

- **Does it need a choice that the code does not determine** (a new name, a target module, a default argument, the body of a new case)? Then it is a refactoring.
- **Is it the one edit that removes an error?** Then it is a fix: `write-raises` for `UndeclaredRaise` and `UntypedRaise`, `_` for an unused pattern variable once T3 is an error, the removal of an unused import (Checker 3), the removal of a handler that is `NeverRaised`, a qualified name for an `Ambiguous` use where only one candidate checks in its context, and the hints of Diagnostics 2 for the habits of other languages (`x := e` to `assign x := e`, `[1, 2]` to `List(1, 2)`, `s + t` of strings to `s ++ t`).
- **Is it determined by the code, meaning-preserving, and better by a cost that every reader shares?** Then it is a canonicalization, for lint. To be applied without review it must also be terminating (the cost strictly decreases), confluent (the order of applying rules does not matter), and idempotent, as `format` is; the e-graph of `lint.md` gives the first two by construction, since extraction picks one cheapest representative per class.
- **Is it only faster, and less clear to read?** Then it is an optimization and never edits the sources: fusion of comprehensions, which `ulang/compile/compile.u` already does by nested loops, unrolling, constant folding, a fold that `size(List(for …))` counts instead of building the list, sharing of repeated pure subexpressions, and the extraction by the cost of evaluation (`lint.md`, step 6 of its order). Rewriting the source for speed would undo the canonical form that lint establishes.

So of the operations above, two are no operations of their own, but edits that `refactor fix` applies: `hoist-locals` is `LocalTooHigh` of `ulang/lint/lint.u` (the style rule fixes the place, so no choice is left), and `write-raises` is a fix. `expand-wildcard` stays a refactoring, which F4 reports and offers as its fix, since whether a `case _` should be expanded depends on whether the datatype grows, which the project declares (below).

### Canonical forms missing from `lint.u` and `lint.md`

Candidates beyond R1–R7, D1–D3, and S1–S3, each determined by the code and each a rewrite with a test before and after:

- **Booleans and conditionals**: `match c case true => a case false => b end` to `if c then a else b`; `if not c then a else b` to `if c then b else a`; `if c then skip else s end` to `if not c then s end`; `if a then b else false` to `a and b`, `if a then true else b` to `a or b`; `not exists(xs, p)` and `forall(xs, not p)` to one of them.
- **Empty containers and identities**: `size(xs) == 0` to `xs == List()`, also of sets, maps, and strings; `xs ++ List()`, `List() ++ xs`, `s ++ ""`; `e with f := e.f`; `let x := e in x` to `e`.
- **Lambdas**: `lambda x => f(x)` to `f` where `f` is one declaration, not an overload that the lambda's type resolves, which the comparison of resolutions decides; a `foldr` or `foldl` whose step only conses or adds to a set or a map to the comprehension.
- **Maps**: `for (k, v) in List(for x in m)` to `for k => v in m`, the rule that `CLAUDE.md`, Style, states from a mistake (`field_table_decls`); `match get(k, m) case Some(v) => v case None => d end` to `value_or` (R2, on the branch); `if contains(k, m) then … get(k, m)` (D3).
- **Statements**: a `try` whose only handler raises what it caught, unchanged, to its body; a local whose initial value every path overwrites before reading it, to a declaration without a value; `match` over a pair of one case to the `let` of R7.
- **Names by the rules of style**: a pattern variable that binds a field under another name, where the field's name is free, to the field's name (Style: "patterns bind the names of the fields"); numbered names (S1) are reported but not renamed, since the new name is a choice, which `rename` then carries out.
- **Order of declarations**: the definitions of `implementation` in the order of their signatures in `export`, which keeps diffs of the two sections parallel; only if the order is not used as documentation, so opt-in.

### Smells that are reported, not rewritten

Their fix is a choice of data structure or algorithm, so lint names the site and the cost, and the agent decides:

- `contains(x, xs)` of a list inside a loop or comprehension over another container, quadratic, where a `Set` would do;
- `at(i, xs)` or `get` of an association list in a loop;
- `assign s := s ++ t` of strings in a loop, where `join` of a `Buffer` would do, like R3 for lists;
- `foldr` over a list that grows with the input, which recurses as deep as the list (the stack is unlimited, but the depth is still memory);
- the same `get(k, m)` repeated in one body, which is a `let` or a `match` once.

### What connects them

- **One frame of edits, two owners.** The rule and its edit stay together, the application of edits does not:

  | Part | Owner | Why |
  |---|---|---|
  | find a place and the edit to make there | the rule, in `ulang/lint/lint.u` or the checker | both come from the same match, so the place found and the edit cannot drift apart |
  | the edit, a part replaced, a statement or command inserted, moved, or deleted, a name renamed | a datatype of the package of `refactor` | lints, the hints of errors, and the operations of agents make the same kinds of edits |
  | apply, print by `format_program`, check again, compare the resolutions, write or diff | the package of `refactor` | one safety check for every source of edits, and the hard part, safety under overloading |
  | what to show | the driver | `lint` prints a warning with its edit, `refactor fix` carries it out, and a reply of a script lists the follow-ups it applied |

  So a finding of lint and a fix carry their edits as values of that datatype, instead of the one expression of `Warning.replacement` now, and `refactor fix` takes them through the same preconditions, the re-check, the comparison of resolutions, and the replay under concurrent edits; lint never writes files by a path of its own, and has no `--apply`. The rules on `walks` without a replacement show which edits come first: `LocalTooHigh` moves a declaration into the branches that use it, `UnusedPrivate` deletes a command, `UnusedVariable` writes `_` or drops a local; their warnings that came with the merge of main at `0d44ab2` stay in the sources as the first cases: `LocalTooHigh` of `ts` and `t` in `ulang/check/check.u`, of `imported` in `ulang/parse/parse.u`, and of `typed` in `ulang/ulang.u`; `UnusedPrivate` of `starts_at` in `ulang/parse/lex.u` and of `ends_body_token` in `ulang/parse/parse.u`; and `UnusedVariable` of `imported` in `parse_mod`. The script that moved 84 locals by the warnings of `LocalTooHigh` on `walks`, by the text, shows the edit: the declaration leaves its `local` line and joins, or opens, the first `local` line of each branch that names it.
- **One matcher.** The rewrites by pattern of operation 6 are the rules of lint written in ulang syntax: the library of canonical forms is a set of rules shipped with the tool, and a project adds its own for a migration, which is a lint rule that runs once.
- **Project policy in `project.u`.** What the rules would hard-code, the datatypes that grow (`Cmd`, `Expr`, `Stmt` for F4, Diagnostics 5), which rules are on, and the threshold of R4, are fields of `Project` of `ulang/prelude/os.u`, as the directories that run only compiled already are.
- **An enforced tier.** Rules that are terminating, confluent, and uncontroversial can be enforced as `format` is: `check` fails on a part that is not canonical, and `make reformat` applies them by `format` and `refactor fix --apply --rule …` of those rules, so agents never chase them as warnings. The rest stays opt-in, which answers open question 4 of `lint.md` per rule rather than for all.

## Output for agents

- **A script applies, `--dry` only reports** (The command, 4): the reply shows what changed and the diagnostics that changed, not the diff, which costs more context than the agent needs to go on.
- **Each refusal names the site and the reason** in the terms of the errors (`HiddenLocal at 12.5`, `uses private parse_type`), and a suggestion (`--with-deps`, a different name), since an agent learns the tool from its messages.
- **Batch by script**, actions applied in order on the tree of the previous one, one check per unit at the end and a report per action, so that a migration of 20 renames is one run of about a second instead of twenty.
- **Exit status**: 0 applied or applicable, 1 refused, 2 usage, as for the other commands.
- **The VS Code extension** gets rename (F2) and the code actions of operations 2 and 3 from the same commands with `--json`.

## Concurrent edits

Several sessions edit the same files at once, also `CLAUDE.md` and `ideas/`, and each commits only its own hunks (working rules of `CLAUDE.md`). A refactoring reads many files, checks for a second, and writes many files, so another session can write in between, and a rename that missed the uses another session just added breaks its build. What helps, from cheap to thorough:

1. **Operations are intent, not text.** A refactoring is stored as what it does, `rename prelude/set.size/1 cardinality`, addressed by names, not by positions, so it can be run again on any later version of the files. That is the main lever: conflicts are resolved by replaying the operation, not by merging its diff. Targets by position, `file.u:L.C`, are turned into names when the operation is planned, and a script keeps the names.
2. **Optimistic writes.** `--apply` records the hash of each file it read; just before writing, it reads them again, and if any changed, it replays the operation on the new text (preconditions, edit, check, comparison) and tries again, a few times, then gives up with the files that keep changing. Each file is written to a temporary file and renamed, so no reader sees half a file, and the check–compare–write window is guarded by a lock, `out/refactor.lock` by `flock`, held only for the comparison and the writes, never for the check, which the replay repeats instead. Other tools that write sources, `format` and the editor, take the same lock or the same hashes.
3. **Uses added after the rename.** A replay finds them, since `refs` is by resolution. For sessions that are still writing code against the old name, `rename --keep-old` leaves the old declaration as a forwarder, `function size[A](s: Set[A]): Int := cardinality(s)`, marked by a doc, which lint reports and `refactor remove-forwarders` removes later by inlining; the transition then needs no coordination.
4. **Uncommitted edits of others that do not check.** The precondition "the units check" then fails for reasons that are not the agent's. `--base HEAD` runs the operation on the committed version, `git show HEAD:file`, in memory, and replays it on the working tree, where only the files the operation touches need to parse, and the result per file is either applied or reported as a conflict; the hunks of the operation are its own, which matches the rule of committing only one's own hunks, and `--patch out.diff` writes them for `git apply --cached`.
5. **Merging at the level of declarations.** Since `format` is canonical, two versions of a file differ by whole commands far more often than by lines within them, so a three-way merge of the parsed trees by command (base, ours, theirs; a command changed on one side only is taken, on both sides is a conflict, compared by `strip_cmd`) resolves what a line merge reports as a conflict only because a neighbouring line moved; `ulang.sh merge base ours theirs` as a git merge driver for `*.u`, also usable by sessions that do not refactor. It needs the diff of declarations of `lint.md` (P1).
6. **A journal.** Each applied script is appended to `out/refactor.log` with its session, time, files, and the text of the files before it, so that `@undo n` restores them, as a replay of the inverse where other edits followed, and `refactor log` tells a session which changes in the working tree are its own when it commits, and which operations of other sessions touched the files it is about to edit.
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

5. **A hole, `todo`**, an expression of any type and a statement, which raises `Todo` at runtime, is an open goal for the prover, and whose remaining uses `test` lists, so a draft checks before every case is written, and a new constructor is stubbed in each match by `@put into #h` of `case Scroll(_) => todo`.
6. **An optional inline export**, `export function f(…): T := …` beside the sections, so that a fragment is the whole truth of a declaration; optional, since the tool keeps the sections in step anyway.

What can stay strict: with the tool, locals before statements, the canonical format, the signatures that `implementation` repeats, and the order of modes cost an agent nothing, so they stay, and docs that are nodes of the tree move with their declaration, so no structural edit loses them.

## Order of implementation

1. `resolve`, `refs`, `outline`, `names`: queries only, by position on the checked program; useful at once and the base of everything else.
2. The frame of `refactor`: the datatype of edits, preconditions, tree edit, `format_program`, re-check, comparison of resolutions, the script as one transaction with `--dry`, replies with handles in `@show` and in diagnostics, with actions addressed by names and replayed on hashes that changed, the lock, and the journal; with it `@show`, `@put` of whole declarations, `@drop`, `@patch`, `@check`, and `@undo`, which carry most of the value: no failed text matches, no unformatted code, follow-ups as a list. Needs Changes to ulang, 2.
3. `@put` of nodes by handle, with the hoisting of locals.
4. `refactor fix` for the lints: `Warning` carries edits instead of a replacement, the expression rewrites first, then the edits of statements and commands of `LocalTooHigh`, `UnusedPrivate`, and `UnusedVariable`, tested on the warnings left in the sources; this needs only the frame, not the queries of step 1 beyond the check.
5. The record of resolutions, Changes to ulang, 1, then `@rename` of declarations, fields, locals; then of modules with docs.
6. The mechanical follow-ups, the hints of fragments with `@retry`, `expand-wildcard`, and the fixes of errors in `refactor fix`, `write-raises` and the others: small, and each the fix of an existing error.
7. The changes of ulang 3 to 6, at any time, each making step 6 smaller.
8. Signature changes and datatype changes beyond `@put`, if the follow-ups prove too many: `tuple-to-record`, `introduce-alias`, `function-to-procedure`.
9. `move`, `export`, `unexport`.
10. Rewrites by pattern, shared with lint.
11. `extract-procedure`, `to-case`, as needs show; `hoist-locals` is `LocalTooHigh` of step 4.

Each operation has tests in `ulang/test/` of the source before and after, and its refusals as cases of `ulang/test/errors/`.
