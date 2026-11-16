# ulang

A small programming language with pure functional expressions, imperative statements, algebraic datatypes, and Hindley-Milner type inference.
It is implemented in itself: an interpreter and a compiler to C written in ulang, where the compiler compiles itself.
The first implementation, an interpreter in Scala 3, is on the branch `scala-baseline`.

This program sorts numbers by inserting them into a search tree:

```
data Tree[A] :=
  case Leaf
  case Node(left: Tree[A], key: A, right: Tree[A])

function insert(k: Int, t: Tree[Int]): Tree[Int] :=
  match t
    case Leaf => Node(Leaf, k, Leaf)
    case Node(l, j, r) =>
      if k < j then t with left := insert(k, l)
      else if j < k then t with right := insert(k, r)
      else t
  end

function keys[A](t: Tree[A]): List[A] :=
  match t
    case Leaf => List()
    case Node(l, k, r) => keys(l) ++ List(k; keys(r))
  end

// inserts the elements of xs into the search tree t, one at a time
procedure insert_all(in xs: List[Int], ref t: Tree[Int]) :=
  for x in xs do
    t := insert(x, t)
  end

procedure main() :=
  var t := Leaf
  insert_all(in List(3, 1, 2, 1), ref t)
  os_println(in show(keys(t)))

test sorting() :=
  var t := Node(Leaf, 2, Leaf)
  insert_all(in List(3, 1, 2), ref t)
  assert keys(t) == List(1, 2, 3)
```

## Language

`LANGUAGE.md` describes the language for writers of programs, by example, starting with the habits of other languages that fail here.

- Functions are pure expressions, procedures are statements with `in`, `out`, and `ref` parameters.
- In a body, `let p := e, …` binds names once, where patterns take values apart, `let (q, r) := (n / 2, n % 2)`, and `var x := e, y: T` declares variables, also without a value, both anywhere in the body and each seeing those before it, as the bindings of a `let … in e` do; a call is `p(in a, out b, ref c)`, where `out _` drops a value that nothing needs, and an assignment `x := e, y := f`, whose right sides are all read before any is assigned.
- Every pattern may carry its type, `lambda p: Point => p.x`, `case Some(n: Int) =>`.
- Functions and procedures are values: `lambda x => e`, whose formals may be patterns, as in `lambda n, (a, b) => n + a * b`, and `lambda in x, out y => statements end` of a type `(in A, out B)`, which sees the variables around it but assigns only its outs and refs.
- `if` always nests: an `if` after `else` is an `if` in its body, which has its own `end`.
- Datatypes with pattern matching, guards, and checks that matches are exhaustive and cases reachable, where alternatives share a case, `case TPlus | TMinus =>`, also nested, `Some(1 | 2)`, each binding the same variables.
- Named fields of constructors, read as `e.field` and updated as `e with field := a, other := b`, which are resolved by the type of `e`, and raise `MissingField` for a constructor without that field.
- Hindley-Milner type inference, where only declarations are polymorphic, with type parameters, and `let` does not generalize.
- Exceptions, values of datatypes without type parameters, raised by the statement `raise e`, and caught by `try`, which leave the variables as they were: outs and refs reach the caller only when a procedure returns, and handlers see the variables as they were before the `try`.
- Checked exceptions: `raises T, …` after an exported procedure and in procedure types, inferred for private procedures with formals and for procedure values bound without a type, while an entry, a procedure without formals such as `main`, raises only what its `raises` declares, where a procedure value of a type that raises less fits a place that allows more, a `try` removes the types that its handlers cover, and a handler of a type that its body cannot raise is an error.
  Builtins raise `DivisionByZero`, `OutOfBounds`, `NoCharacter`, and `NoByte`, which no `raises` counts, whereas a failed `assert` ends the program.
- Unbounded integers, characters, which are Unicode code points, strings of them, which files hold as UTF-8, and bytes, `byte(n)` from 0 to 255.
- Programs read their arguments with `os_args`, files with `os_read_utf8` and `os_write_utf8` as text, or with `os_read_bytes` and `os_write_bytes` as an `Array[Byte]`, streams such as standard input by lines or bytes with `io_open`, `io_stdin`, `io_read_line`, and `io_read_bytes` of the module `io`, and print strings with `os_print`, all of the module `os`, where the builtin `show` gives the text of any value, which a datatype may declare for itself by a function `show`.
- The forms `List(1, 2)`, `Set(a, b)`, `Map(k => v)`, and `Array(a, b)`, and their generators `List(for x in xs if x > 0 yield x * x)`, `Set(for x in c yield f(x))`, `Map(for k => v in m yield k => f(v))`, which range over lists, sets, arrays, or maps by their folds, and `==` compares their elements and entries; and the statement `for x in xs if p do … end`, which runs its body for the elements of any container.
- User-defined operators by `infix`, `prefix`, and `postfix` notations.
- Comments from `//` to the end of the line, anywhere, which `out/ulang.sh format` keeps where they stand, where the comments right above a declaration are its doc.
- Modules and packages: every `.u` file is a module, and the modules of a directory are a package, whose members see each other's exports without imports, also cyclically, while their private declarations stay private to their file. `import p.q` makes the exports of the package of the directory `p/q` available, and is an error if the module uses none of them, by its path from the root of the project, the nearest directory above with `project.u`, but not transitively; packages do not import each other cyclically. A file without sections exports nothing, and its procedures without parameters can be run, and every module sees the whole library without imports.
- Named tests `test name() := …` of `assert` statements, where `assert raises p do … end` expects its body to raise an exception that `p` matches, and `assert raises p if g do … end` one for which the guard `g` also holds, while programs start at a procedure without parameters, like `procedure main() := …`.
- Examples of functions and procedures, lines after the `:=` of the signature and the contract of a procedure, before the body, such as `example gcd(12, 18) == 6`, `example chr(-1) raises NoCharacter(-1)`, or `example divmod(in 7, 2, out 3, 1)`, which run as tests `gcd.example.1`, … in both backends.
- Lemmas `lemma l(in x: A, …) := requires a ensures b`, which `ulang.sh check --prove` proves and `ulang.sh check --test` first tests, as `l.lemma`, on all the values of the formals up to a small depth, with `Int` for type parameters, reporting a counterexample.
- Contracts of procedures, `requires a, …` and `ensures b` after the `:=`, and lemmas that claim them, `lemma sum_correct(in n: Int, out s: Int) := correct sum(in n, out s)`, with an `invariant e` per loop of the body as the proof, which `ulang.sh check --prove` proves for the runs that end, assuming the contracts of the procedures called.

The library in `ulang/prelude/` contains the prelude with natural numbers, options, and lists, as well as maps, sets, and access to files.

## Usage

Requires a C compiler, GMP, and the Boehm garbage collector, with their headers, the packages `libgmp-dev` and `libgc-dev` on Debian and Ubuntu, `gmp` and `gc` on Arch. The interpreter and the compiler recurse as deeply as the programs they run, so `out/ulang.sh` runs `out/ulang.bin` with a large stack, by `ulimit -s unlimited`; compiled programs need the same limit.

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
out/ulang.sh refactor script.txt          # runs a script of edits by declaration, see Refactoring
out/ulang.sh help                         # lists the commands
cc -I ulang/runtime file.c ulang/runtime/ulang.c -lgmp -lgc
make test                                 # runs all tests
```

## Refactoring

`out/ulang.sh refactor` edits sources by their meaning, as `format` edits their layout. It runs a script, from a file, from stdin, or as actions on the command line, a line of the script from each word that starts with `@`, `out/ulang.sh refactor @show shapes.u:area @refs shapes.u:area`, where the last one, if it is a `@put` or `@patch`, takes its fragment from stdin. Each action is a line that starts with `@`, which ulang never uses. Its fragment is the lines up to the next action, taken verbatim, where a line that starts with `@@` is a line of the fragment with one `@`, which a fragment of `@patch` never needs, since its lines start with `-` or `+`; so an action that takes a fragment and has none, since the line after it starts with `@`, is refused with the line that it took as an action, and an unknown action with the escape `@@`. A script writes all its files or, if one action is refused, none.

`@show` prints code with handles, `#n` in a margin beside each line that starts a node: a statement, a case, a constructor, or an expression that starts its line, the body of a function, the body of a `let` after `in`, a branch of an `if` spread over lines, an element or argument of a form or call spread one per line, or a line of a spec, an example, `requires`, or `ensures`, where those of a signature in `export` are nodes of its definition, and the head of the definition of a function or procedure and of each of its signatures in `export`, up to its `:=`, which a diagnostic at the head, such as `SignatureMismatch`, names, and which only `@put N` replaces, where the signature in `export` follows the new head of a definition, while a head of a signature is replaced alone and a head of another name or arity moves the declaration with its handles to its key, `replaced F:p/3 #4, now p/2`. A node is its text from its first token to its last, so what follows it on its last line, such as the comma after an element or the `)` after the last one, stays where an edit replaces it; `@show` of a node and the echo of an edit also print the comments that `format` places above it, which an edit of the node keeps.

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

Fragments need no layout, since every edited file is printed by `format`. The first line of the reply is the state after the script, `applied, clean`, or its errors and warnings, each with how many of them the script made new if it changed a file, `applied, 2 errors (1 new), 1 warning (0 new)`, and its tests; then what each action did, with the nodes that an edit by handle placed, and the diagnostics and lints that are new (`+`) or gone (`-`) in the files that it checked, where one that only moved, with the lines that its message names, is neither, each new one with the handle of its node. It checks the files that it changed and those of `@check`, each with the packages that it imports only as far as it sees them, so that an error in a definition of an import is listed only by a script that checks a file of that package, but not the importers of a changed package: a script that changes the declarations of an export or builtin section names them in a line of their own, `a/a.u: the exports f changed, whose importers are not checked`, since those may now have errors, where a changed datatype names itself, the constructors whose lines changed, and the fields whose types or positions among the fields of a constructor changed; and each comment line that it wrote longer than the 120 columns to which comments are filled, which `format` keeps as written, `a/a.u:3: a comment line of 152 characters, longer than 120, which format does not wrap`:

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

Without the `@put into #2`, the reply would have listed the match that the new constructor leaves open as a new diagnostic, so the follow-ups of a change are a list of work, not a search. The status is 0 if the files are clean or have warnings only, 1 if errors remain or the script is refused (`rejected`, with the message of the refusal, and for an error of the system, such as a file that cannot be read, the line and the action of the script), and 3 if a test failed. A test that fails by reaching an error of the reply says so instead of repeating it, and the tests and examples that errors keep out of the program are counted as not run, as are the examples on a definition that belong to its signature, while the lemmas whose tests they keep out are named.

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

A declaration is a target `F:name`, or `F:name/n` of the arity `n` among overloads, or `F:name/test`, `F:name/lemma`, or `F:name/type` of that kind, which has no arity, so that a procedure `coverage/4` and a test `coverage` are told apart; an action that takes one declaration refuses a target that names several, with each of them, its kind, and the target that names it alone, `@patch takes one declaration, while F:coverage names 2: the procedure coverage/4, which F:coverage/4 takes, the test coverage, which F:coverage/test takes`.

A node N is a handle `#n`, a handle scoped by its declaration `F:name #n`, which is refused if the handle lies elsewhere, the node of a declaration that starts with a text, `F:name "case Listing"`, or the node at a position, `F:L.C`. The text is all between the first and the last quote of the line, read with the escapes of string literals, `F:name "case List(\"fetch\")"`, and else as written, so that `F:name "case List("fetch")"` names the same node.

| Action | What it does |
|---|---|
| `@show T` | prints a file, a declaration `file.u:name[/arity]`, or a node N, with handles |
| `@outline F` | one line per declaration of `F`: its lines, section, kind, name and arity, whether exported, its handle, and the constructors of a datatype |
| `@sig name` | the exported declarations of the name in the modules of the project and the library, across overloads, each by its qualified name: a signature with its raises and its examples, which it leaves out beyond 4 lines, a datatype whole, and a constructor under the head of its datatype |
| `@sig M` | the export and builtin sections of a module, a file `F` or a path from the root, `ulang/check/check`, or `prelude/str` of the library, as the file writes them, with their docs |
| `@resolve F:L.C`, `@resolve F L.C` | the qualified name and type of the use at the position, or the type of the variable declared there, from `hints` |
| `@refs F:name[/n]` | the uses of a function, procedure, pattern alias, or constructor in its package and its importers, by resolution, each with its position, declaration, handle, and line |
| `@put F` | writes the declarations of the fragment into `F`, each replacing the one of its name and arity where it stands, whatever section the fragment names, or added, which the reply says with its section, naming each declaration once, and the other arities of its name that the file declares, which it overloads, `added g/0 to implementation, beside g/1`, and each import that it added, `added import p.q`, and says `of another type` where a replacement's types differ; a missing or empty `F` is the fragment, header, imports, and sections, in a directory made if missing; into a file, the `import` lines join its imports and the declarations after `export` or `builtin` go into that section, which is made if missing; an import of the fragment replaces an import of the file of the same last name that fails, as of a package that moved, a declaration keeps its doc unless the fragment writes one, the signature in `export` follows the head of its definition and, if the fragment writes no signature, takes the doc of the definition, unless the file's definition has a doc of its own, which the fragment's then replaces, or the definition is a datatype, whose abstract type in `export` keeps its doc, for its importers, and its examples and contracts, in place of those of the signature, `the examples of f/1 on its signature`, a definition follows the head of a signature in `export` that the fragment writes without the definition, also of another arity and of a private definition that it exports, `with the head of the definition of f/1`, a private procedure that the fragment exports gets the raises of its signature or else those that the check inferred in both heads, as `@export` does, and a comment of no declaration is refused |
| `@put F:name[/n]` | the declaration of that name in the fragment replaces the target at its place, whatever its arity, with its signature; of the others, each that the file declares by its name and arity replaces it where it stands, as for `@put F`, and the new ones follow the target in their sections, in the order of the fragment, also one of a name that the file declares with another arity, which is added beside it as an overload, each named by its place, after the target or the one added before it, `added g/0 after f/1, beside g/1`, so that a declaration of the old arity is dropped by `@drop F:g/1`, while one that replaces a declaration other than the target says where that stood, `replaced h/0 at line 7, which @put F:f does not target`, so that a helper of the fragment that meets a declaration elsewhere in the file is told from the additions, rather than refused, since replacing a helper together with its caller is the common case and `@undo` takes it back; a fragment of one declaration of another name, of the target's kind, replaces it so too, while the uses keep the old name, which `@rename` changes |
| `@put [F] before T`, `@put [F] after T` | places the new declarations of the fragment next to the declaration `T` in its section, which the reply names, `added b/0 after a/0`, where the lines under no section go into the builtin section if `T` is a builtin, and into the export section if they are signatures or abstract types, which only it takes, and `T` has a signature there, where `T` is `F:name[/n]`, so `@put F after name` is refused with the form `@put after F:name` |
| `@put N`, `@put before N`, `@put after N`, `@put into N` | replaces the node, or inserts code next to it or as its last statement or case, at its indentation, refused for a fragment of another kind than the node, a statement, a case, an expression, a constructor, or a head, which nothing goes next to or into, or one that writes declarations; a fragment may keep the margin of `@show`, and writes the node alone, without the comma or `)` after it, which stay, while one before or after an element gets the comma between them, also after the last one, and a drop of the last element moves its `)`, where a comment at the end of a line stays with the element that it follows; a drop of the last line of the spec of a signature drops its `:=` |
| `@drop T` | removes a declaration, with its doc and signature, saying the sections where it had more than one, or a node N |
| `@patch T` | edits the text of a declaration, its signature and definition alike, of a node N, or of a file `F`, by hunks written as in a diff, see Patching below |
| `@example F:name[/n]` | adds the examples of the fragment, lines `example …`, after the spec of the function or procedure, on its signature if it has one, the place of the examples of an exported name, else on its definition, and shows them with their handles |
| `@rename T new` | renames a function, procedure, pattern alias, or constructor at its declarations and at every use that resolves to it, several per script, with a warning where the new name meets declarations of any arity that the uses also see |
| `@export T`, `@unexport T` | moves a definition's doc, contract, and examples to a new signature in `export`, beside the signature of the exported declaration next to the definition, with the raises that a procedure infers, or back, refused while another module uses it; a datatype or an alias moves whole |
| `@fix T [Rule …]` | applies the edits of the lints of a file or a declaration, all or of the rules named |
| `@undo [n]` | restores the files of a script of the journal, by default the last one |
| `@check [test] [F]` | checks `F` too, and with `test` runs the tests of the files, whose results the reply lists, while what the tests print goes to stderr, so that the first line of the output is the state, but not those of a directory that `project.u` runs only compiled, whose files it names, and names the tests of their packages that the errors of an earlier script left out and that no script has run since, `not run since an earlier script, whose errors left them out: a/wire.u: test round_trip` |

- A handle stays valid across edits of other declarations, and across the edits of the same script, also of `@patch`: an edit keeps the handles of the nodes before and after it at their places, also among identical statements, and a node replaced by its own text keeps its handle and those inside it. It becomes stale when its node is changed by something else, and `@show` gives a new one.
- `@rename` finds the uses by the names that the checker resolved, not by their text, so it never renames another overload or a local. After its writes, it checks every file of the package and every importer that contains the name, and undoes the script if any use resolves differently, e.g. to a local of the new name. It lists the mentions in `CLAUDE.md`, `README.md`, and `ideas/` that it leaves to you.
- `@fix` rewrites expressions, drops an unused private declaration, writes `_` for an unused pattern variable and at the outs that only feed an unused local, removes an unused local, makes a `var` that nothing assigns a `let`, and moves the declarations of a `var` that nothing assigns into a `let` above it where the others are assigned, and moves a local that is declared too high into the branches that use it. A fix that enables another takes a second `@fix`. An edit after which the unit of the file has an error that it did not have before is refused and listed in the reply with that error, `f.u: refused OptionMatch at 5.3, which would cause f.u:5.3: …`, while the others apply: the edits are checked together, and if that adds an error, each in turn on top of those taken.
- An error in a fragment names its line in the fragment and in the script, `fragment 2.6, script 3.6`. Where the fragment has an error, the errors at the lines of the file, which parsed before, are its follow-ups and left out, as are those after a keyword written as a name on its line, which the parse took as the start of a section or a command: a field `builtin` replies only `fragment 2.29, script 3.29: keyword builtin is not a name`.
- A script edits files that parse, also with an import that fails, such as one of a package that moved, which a `@put` of the new import replaces.
- Paths in actions are normal, `./a.u` is `a.u`, and the handles are kept by their paths from the root, so that they hold from any directory.
- `--dry` writes nothing, but checks the texts that it would write, so its reply, `dry, 2 errors (2 new)`, lists the diagnostics that the script would leave, without handles, since their nodes are in no file, and without tests; the journal and the handles are kept in `out/refactor/` of the root, and the queries write nothing but the handles they show.
- The actions and the checks of a script see the files that it has written so far, also a module of a new package, which another file of the same script may import.
- `--overlay F=<path>`, repeated for several files, takes the text of the file at path as that of `F`, as an editor passes its unsaved buffers; the script writes `F` only if it changes it.
- A query that is refused, `@show`, `@outline`, `@refs`, `@sig`, or `@resolve`, takes no other action down with it, since it writes nothing: the reply says `@show T: refused, …` in its place, its first line counts it, `applied, clean, 1 query refused`, and the status is 1, while any other action that is refused refuses the script.


### Patching

`@patch T` takes hunks as a diff writes them: a run of lines that start with `-` is an old text, and the run of lines that start with `+` right after it is its new text. The marker and one space after it are stripped, and the rest of each line is taken as written, `=>` and all, so a line of a `case`, a lambda, or a map patches as any other:

```
@patch shapes.u:area
- case Circle(r) =>
+ case Circle(radius) =>
-3 * r * r
+3 * radius * radius
```

- **Hunks.** A blank line ends a hunk, a `-` run without `+` lines deletes its text, and a line that starts with neither marker is refused with the form. The hunks apply in order, each to the text that those before it left, so a hunk may anchor on what one before it wrote; applying all against the original would make two hunks of one text conflict.
- **Matching.** The old lines are compared without their indentation, as whole lines, or, for a hunk of one old line, as a part of a line; of several old lines that occur nowhere as whole lines, the first may be the end of its line and the last the start of its line, whose rest stays around the new lines. An old text occurs in the target, and at most once in each of its parts: the signature and the definition of a declaration, so that a text of both heads changes in both, or each declaration of a file and each run of its lines of no declaration, the comments above the imports included. Old lines of a file that span declarations are matched once in the whole file.
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
- **Refusals.** A hunk whose new lines are its old ones, and a patch that changes nothing once formatted, are refused. A hunk whose old text occurs nowhere or too often says so, by its fragment line where the patch has several hunks, with the parts where it occurs, the hunk before it that changed it, the earlier action of the script that removed it from the target, the declarations of the file that have it, the earlier action that removed it from the file, and else the line of the target that starts most like it. A patch whose text does not parse is refused as `@patch T writes a text that does not parse`, with the fragment lines of its new lines.
- **Reply.** A patch of a file names the declarations that it changed. The reply shows the nodes that the patch changed, with their handles, those of each run of changed lines, so that the hunks of a patch show their own nodes and not those between them, where a changed comment counts as the node below it, which it documents, and a changed line of a `let … in` as the `let` without its body, unless the body changed too.

## Layout

- `ulang/` is the implementation in ulang, a package per phase, each importing those before it: `ulang/syntax/` with the errors that all phases share, `ulang/format/` the printer, `ulang/parse/` lexer and parser, `ulang/check/` the checker, `ulang/run/` the interpreter, `ulang/compile/` the compiler to C, with its runtime in `ulang/runtime/`, `ulang/prove/` a prover by contextual simplification over checked programs, `ulang/lint/` the lints, `ulang/refactor/` the scripted edits of `refactor`, and the driver `ulang/ulang.u`.
- `bootstrap/ulang.c` is the seed, the C of the last compiler that compiled itself to the same C, from which `make` builds
  `bootstrap/ulang.bin`, which compiles the sources to `out/ulang.bin`. `make selfcompile` replaces the seed by the compiler
  compiled with itself, and `make bootstrap` by the compiler compiled with the interpreter. `bootstrap/ulang.sh` is the script
  that runs each binary with a large stack, copied next to it by `make`.
- `ulang/prelude/` is the library.
- The tests of each package are in its subfolder `test/`: `ulang/test/` of the language, `ulang/prove/test/` of the prover, and `ulang/prelude/test/` of the library. No file fails: the errors of the phases are tested in `ulang/test/errors/`, by cases, source texts in tests, whose exceptions `assert raises` expects.
- `vscode/` is an extension for VS Code and VSCodium, installed by `make install`.
- `ideas/` holds the open work and the plans, listed in `ideas/index.md`, and `DESIGN.md` the design notes.
