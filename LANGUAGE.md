# Writing ulang

The language for writers of programs, by example. `CLAUDE.md` describes the implementation, and `README.md` the tools. Every block marked `u` here is a file that `make testdocs` checks and whose tests it runs, so the examples are formatted as `ulang.sh format` writes them; the other blocks are fragments.

Check after each small edit, `ulang.sh check --test file.u`, and let `ulang.sh format file.u` write the layout, which `check` requires. The errors are JSON lines, and for most habits below they give the form to write after `; `.

## Habits from other languages that fail

- No `return`, `break`, `continue`, or `+=`: a procedure gives its results by its `out` formals, and a loop ends by its condition or an exception.
- `let x := e` binds a name once, which nothing assigns, else `x is not assignable`; `var x := e` declares a variable. Both stand anywhere in a body and hold to its end.
- The bindings of one `let` or `var` are sequential, `let a := 1, b := a + 1`, but the assignments of one statement are simultaneous: `x := y, y := x` swaps, and `y := f(x)` reads the `x` from before.
- `let x := e in body` is an expression; in statements `let x := e` stands alone, without `in`.
- `if c then … end` and `while c do … end` in statements; `if c then a else b` in expressions always has its `else`. There is no `elif`: `else if` opens an `if` of its own, with its own `end`.
- Containers are forms: `List(1, 2)`, `List(x; rest)`, also in patterns, `Set(…)`, `Map(k => v)`, `Array(…)`, never `[1, 2]` or `x :: rest`. `[…]` is only for type arguments, `List[Int]`.
- Tuple types are written as tuples are, `(Int, Str)`, also `List[(Int, Str)]`. `(A, B) -> C` takes two arguments; a function of one pair is `((A, B)) -> C`.
- Strings join by `++`, never `+`, which is of `Int` only; `show(x)` gives the text of any value.
- `||` and `&&` are no operators: `Bool` has `and`, `or`, and `not`. A `|` is only between alternatives of a pattern, `case TPlus | TMinus =>`.
- Lists have no index operator: `at(i, xs)` is the element at `i`, while `get(k, xs)` looks up the key `k` in a list of pairs. The first element is taken by a `match`, since `xs.head` raises `MissingField` on `List()`.
- Map, filter, and flatten are generators, `List(for x in xs if p(x) yield f(x))`, and `concat(xss)`; there is no `map` or `filter` of lists.
- Evaluation is strict: `or_else(p(cs), q(cs))` computes both, so a parser of alternatives tries the second only in the `None` case of a `match`, and a recursive grammar of combinators must be a function of its input, not a value.
- Within a module a name is overloaded only by its number of formals: a `lower(s: Str): Str` beside a `lower(c: Char): Char` in one module is a clash. Across modules, as in the library, overloads are told apart by their types.
- Functions cannot `raise`, loop, or read files: such code is a procedure.
- A `try` restores every variable that its body changed, also a counter passed by `ref`, so what a failed attempt learned leaves only in the fields of its exception.
- Arrays are immutable, and building one copies it: state that changes by index or key belongs in a `Map`.
- A local, formal, or out hides every function of its name, so `out lines: List[Str]` cannot call `lines(text)`, as the error of such a call says: rename the variable.
- `split(text, '\n')` of a text that ends with a newline gives a last empty string; `lines(text)` gives the lines.
- Keywords are no names, also not of fields, formals, or locals: `_`, `assert`, `builtin`, `case`, `catch`, `correct`, `data`, `do`, `else`, `end`, `ensures`, `example`, `export`, `false`, `for`, `function`, `if`, `implementation`, `import`, `in`, `induction`, `invariant`, `infix`, `lambda`, `lemma`, `let`, `match`, `out`, `pattern`, `postfix`, `prefix`, `procedure`, `raise`, `raises`, `ref`, `requires`, `skip`, `test`, `then`, `true`, `type`, `try`, `var`, `while`, `with`, and `yield`; `and`, `or`, `not`, `show`, and `compare` are names, and `List`, `Set`, `Map`, and `Array` name types, and are forms only before `(`; and the error of a keyword that is a common word names a name to write instead, `keyword raises is not a name; write a name such as raised`.

## Datatypes and functions

A datatype lists its constructors after `case`, one per line, each with named fields. A field name may repeat in several constructors of a datatype, and then must have the same type in each. `e.field` reads a field and `e with field := a, other := b` updates fields, where a constructor without the field raises `MissingField`. A `match` must be exhaustive, its cases reachable; cases with one body share a head, `case A | B =>`, also inside, `Some(1 | 2)`, where each alternative binds the same variables.

```u
data Shape :=
  case Circle(r: Int)
  case Rect(width: Int, height: Int)

function area(s: Shape): Int :=
  match s
    case Circle(r) =>
      3 * r * r
    case Rect(w, h) =>
      w * h
  end

function kind(n: Int): Str :=
  match n
    case 0 | 1 =>
      "small"
    case _ =>
      if n < 0 then "negative" else "large"
  end

test shapes() :=
  let s := Rect(2, 3)
  assert area(s) == 6 and s.width == 2
  assert (s with height := 5).height == 5
  assert kind(1) == "small" and kind(-3) == "negative"
  let width := lambda r => (r: Shape).width
  assert width(s) == 2
```

Functions are expressions: `if … then … else …`, `match`, `let p := e, … in body`, calls, and `lambda x, (a, b) => e`, whose formals may be patterns. Type parameters are declared after the name, `function keys[A](t: Tree[A]): List[A]`, and only declarations are polymorphic: a `let` does not generalize. Every pattern may carry its type, `lambda p: Point => p.x`, `case Some(n: Int) =>`, and so may an expression in parentheses, `(p: Point).x`, `(empty(): Set[Int])`, which gives the type where nothing else does yet, as for a field of the formal of a lambda bound by `let`, or an overload; an ascription stands alone in its parentheses, so a component of a tuple is written `((a: Int), b)`. `-1` is a literal, also in patterns, and `0xFF` is hexadecimal.

Other declarations: a type alias, `type Pairs := List[(Str, Int)]`; an operator, `infix left 11 **` before the `function **(m: Int, n: Int): Int`, also `prefix` and `postfix`; and a pattern alias, `pattern Pair(a: Int, b: Int): List[Int] := List(a, b)`, which matches and builds.

## Procedures and statements

A procedure has formals of the modes `in`, `out`, and `ref`, where a mode covers the formals after it up to the next mode, `in a: Int, b: Int, out q: Int, r: Int`. The arguments of a call go by mode in the same order, `divmod(in 7, 2, out q, r)`, and `out _` drops an out that nothing needs. A procedure works on copies of its outs and refs, which reach the caller only when it returns.

Statements: `let` and `var`, assignments, calls, `if`, `while`, `for x in c if guard do … end` over any container, `match`, `try`, `raise e`, `assert`, and `skip`. A variable is read only where every path assigned it, and every path that does not raise assigns the outs. A `var` without a type gets it from its value, or as an out of the call right after it, `var q, r`, so locals that are outs of several calls are each declared right before their call or with their types.

```u
procedure divmod(in a: Int, b: Int, out q: Int, r: Int) :=
  q := a / b, r := a % b

procedure count_up(in n: Int, ref total: Int) :=
  var i := 0
  while i < n do
    i := i + 1
    total := total + i
  end

test procedures() :=
  var q, r
  divmod(in 7, 2, out q, r)
  assert (q, r) == (3, 1)
  divmod(in 9, 4, out _, r)
  assert r == 1
  var total := 10
  count_up(in 3, ref total)
  assert total == 16
  var x := 1, y := 2
  x := y, y := x
  assert (x, y) == (2, 1)
```

Procedures are values of types such as `(in Int, out Int)`: a declared procedure by its name, or `lambda in x, out y => statements end`, which reads the variables around it and assigns only its own outs and refs, and whose ins may be `_`, values that it does not read, `lambda in _, out y => y := 0 end`. A variable of a procedure type is called as a procedure is.

```u
procedure twice(in step: (in Int, out Int), x: Int, out y: Int) :=
  var z
  step(in x, out z)
  step(in z, out y)

test values() :=
  let inc := lambda n => n + 1
  assert inc(1) == 2
  let double := lambda in n, out m =>
    m := n * 2
  end
  var y
  twice(in double, 3, out y)
  assert y == 12
  assert (lambda (a, b) => a + b)((1, 2)) == 3
```

## Exceptions

An exception is a value of a datatype without type parameters, raised by the statement `raise e` and caught by `try … catch case p => … end`. The handlers see the variables as they were before the `try`. `assert raises p do … end` expects its body to raise what `p` matches, and `assert raises p if g do … end` also that the guard `g` holds, which reads the variables of `p`, `assert raises OsError(text) if text == "cannot read " ++ path do … end`, since `p` is a pattern and cannot compute a value; without a guard, `p` binds no variables, and `_` stands for what does not matter. A failed `assert` ends the program and cannot be caught.

The exceptions are checked. A procedure says what it raises after its formals, `raises Refused, OsError`, which an exported procedure, a procedure without formals such as `main`, and a formal of a procedure type, `step: (in Int, out Int) raises Refused`, must declare, while a private procedure with formals has it inferred. The errors name each place to change, and a handler of a type that the body cannot raise is an error. The builtins raise `DivisionByZero`, `OutOfBounds`, `NoCharacter`, `NoByte`, and `MissingField`, which no `raises` counts, so functions fail only by them.

```u
data Refused :=
  case Refused(reason: Str)

procedure check_age(in age: Int) raises Refused :=
  if age < 0 then
    raise Refused("negative: " ++ show(age))
  end

data Stop :=
  case Stop(seen: Int)

procedure bump(ref n: Int) raises Stop :=
  n := n + 1
  raise Stop(n)

test exceptions() :=
  var n := 0
  try
    n := 1
    check_age(in -1)
  catch
    case Refused(reason) =>
      assert n == 0 and reason == "negative: -1"
  end
  assert raises Refused(_) do
    check_age(in -5)
  end
  try
    bump(ref n)
  catch
    case Stop(seen) =>
      assert n == 0 and seen == 1
  end
```

## Containers and the library

Every module sees the whole library without imports. The forms build containers, and generators map and filter them, `for p in c` over lists, sets, arrays, options, and buffers, `for k => v in m` over maps, with guards `if q`, from left to right, several in one form, `List(for x in xs for y in ys yield (x, y))`. After exactly one generator `yield` may be omitted, which yields what its pattern binds. A form also converts, `Set(for x in xs)`, `List(for k => v in m yield (k, v))`, `Array(for x in xs)`.

```u
test containers() :=
  let xs := List(1, 2, 3, 4)
  assert List(for x in xs if x % 2 == 0 yield x * x) == List(4, 16)
  assert concat(List(List(1), List(2, 3))) == List(1, 2, 3)
  assert Set(for x in xs if x > 2) == Set(3, 4)
  let m := Map("a" => 1, "b" => 2)
  assert get("a", m) == Some(1) and get("c", m) == None
  assert Map(for k => v in m yield k => v + 1) == Map("a" => 2, "b" => 3)
  assert at(1, xs) == 2 and get(1, List((1, "one"))) == Some("one")
  assert lines("a\nb\n") == List("a", "b") and split("a\nb\n", '\n') == List("a", "b", "")
  var sum := 0
  for x in xs if x > 1 do
    sum := sum + x
  end
  assert sum == 9
```

The containers share names and the order of their arguments, the element or key first, the container after it:

- all of lists, sets, maps, and arrays: `size(c)`, `contains(x, c)`, `exists(c, p)`, `forall(c, p)`, and `find(c, p)`, which stop at the first element that decides, `foldr(c, acc, f)`, `foldl(c, acc, f)`, where `p` and `f` of a map take key and value, in the order of a list and ascending for sets and maps;
- `get(k, c)`, an `Option`, of maps, of lists of pairs, and by index of arrays and strings; `at(i, c)` of lists, arrays, and strings, raising `OutOfBounds`; `remove(x, c)` of lists, sets, and maps; `empty()` of sets, maps, and buffers;
- `a ++ b` concatenates lists and strings, unites sets, and joins maps, where the second wins; `a -- b` removes the elements of a list or set, and the keys of a set from a map;
- lists also have `reverse`, `sort`, `distinct`, `zip`, `take`, `drop`, `slice`, `range(i, j)`, `index_of`, `last`, `take_while`, `drop_while`, `span`, `flat_map`, and `concat`; sets `insert` and `intersect`; maps `put(k, v, m)`;
- `Option[A]` is `None` or `Some(value)`, with `or_else`, `value_or`, and `map_option`;
- strings: `chars(s)` and `str(cs)`, `int(c)` and `chr(n)`, `slice(s, i, j)`, `split(s, c)`, `join(sep, strs)`, `lines`, `trim`, `starts_with`, `ends_with`, `index_of(part, s)`, `number(s)` as an option, `lower`, `upper`, and `is_digit`, `is_letter`, `is_space` of characters. A `Char` is a Unicode code point, which `size`, `at`, and `slice` count, and strings have no folds, so a loop over characters goes over `chars(s)`;
- `Buffer[A]` collects the elements of a loop that adds at the end, `b := b ++ List(x)`, read out by `List(for x in b)`;
- `Int` is unbounded, where compiled code allocates every `Int` outside -16 to 255; `/` and `%` raise `DivisionByZero`; `min`, `max`, `abs`, and `gcd`; `first` and `second` of pairs;
- `compare(a, b)` is a total order of all values, `-1`, `0`, or `1`, which `<`, `<=`, `>`, and `>=` follow, and `==` compares structurally, also containers. A datatype may declare its own `compare`, `==`, or `show` in its module, which the builtins then use;
- files and the system, procedures that raise `OsError`: `os_args(out args)`, `os_read_utf8(in path, out text)`, `os_write_utf8(in path, text)`, `os_read_bytes` and `os_write_bytes` of an `Array[Byte]`, `os_list`, `os_walk`, `os_remove(in path)` of a file or an empty directory, `os_remove_all` of a tree, `os_cwd`, and `os_print(in text)`, `os_println`, `os_print_error`, `os_exit(in code)`, `os_env(in name, out value)` of a variable of the environment as an option, and `os_time(out seconds)` and `os_clock(out nanoseconds)`, which raise nothing; streams by `io_open`, `io_stdin`, `io_read_line`, `io_read_bytes`, `io_write_utf8`, and `io_close`, of the module `io`, also connections of TCP by `io_connect(in host, port, out io)`, and by `io_listen(in host, port, out listener)` and `io_accept(in listener, out io)`.

## Modules and packages

A file without sections is a program: its commands are private, and its procedures without formals are entries, `ulang.sh run file.u` calls `main`, `run file.u:go` calls `go`. A module that others use has sections: imports, then `export` with the datatypes, notations, and signatures without bodies that others see, and `implementation` with the definitions, which repeat the exported signatures exactly, and the private declarations and tests. `type T[A]` in `export` is abstract, defined by a `data` in `implementation`, so others see neither its constructors nor its fields.

```
import app.store

export

type Inbox

procedure load(in path: Str, out inbox: Inbox) raises OsError

implementation

data Inbox :=
  case Inbox(mails: List[Mail])

procedure load(in path: Str, out inbox: Inbox) raises OsError :=
  …
```

The `.u` files of a directory are one package: they see each other's exports without imports, also cyclically, while the private declarations of each stay in its file. `import app.store` loads the package of the directory `app/store` by its path from the root of the project, the nearest directory above with a file `project.u`. A use may write the path of a package or a module before a name, `app.store.load(in path, out inbox)`, also in types, `app.store.Inbox`, and patterns, `case prelude.option.Some(n) =>`, where it means the exports of that package, or of the module of that path, among which the types decide as everywhere; no type hides another, also not one of the module a type of the library, so two visible types of one name make an unqualified use ambiguous, and it writes the path of the one it means; and no local may be named as the first name of a path that the module sees, such as `app` or `prelude`. The names of a path are names, letters, digits, and `_`, never `-`, so the files and directories of packages are named so, `mail_store/`. Imports are not transitive, so a module imports each package whose names it uses, also for the types it names; an import of which nothing is used is an error, and so is a cycle of packages. A type of a module hides one of the same name that it imports.

## Tests and specifications

- `test name() := …` of statements, with `assert` and `assert raises`, run by `ulang.sh check --test`. A test with formals, `test reverse_twice(xs: List[Int]) := …`, is a property, run on all small values of the formals.
- An example, `example f(args) == result`, `example f(args) raises E(…)`, or of a procedure `example divmod(in 7, 2, out 3, 1)`, stands after the `:=` of a signature in `export` or of a private definition, before the body, and runs as a test `f.example.N`.
- A lemma `lemma l(in x: A, …) := requires a ensures b` states a property for the prover, `ulang.sh check --prove`, which `check --test` first tests on small values.
- A procedure has a contract `requires a, … ensures b` after its `:=`, which a lemma `correct p(in a, out y)` claims, with an `invariant e` for each loop of `p`.

```u
function clamp(n: Int, lo: Int, hi: Int): Int :=
  example clamp(5, 0, 3) == 3
  example clamp(-1, 0, 3) == 0
  max(lo, min(n, hi))

lemma size_append(in xs: List[Int], ys: List[Int]) :=
  ensures size(xs ++ ys) == size(xs) + size(ys)

test reverse_twice(xs: List[Int]) :=
  assert reverse(reverse(xs)) == xs
```
