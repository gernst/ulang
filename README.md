# ulang

A small programming language with pure functional expressions, imperative statements, algebraic datatypes, and Hindley-Milner type inference, written by agents and designed so that what they write is cheap to verify.
It is implemented in itself: an interpreter, a prover, and compilers to C and to x86-64 written in ulang, where the compiler compiles itself.
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

## Building

ulang requires a C compiler, GMP, and the Boehm garbage collector, the packages `libgmp-dev` and `libgc-dev` on Debian and Ubuntu, `gmp` and `gc` on Arch; their shared libraries `libgmp.so.10` and `libgc.so.1` suffice without the packages for development.

```
make          # builds out/ulang.bin and out/ulang.sh from the sources, by the seed in bootstrap/, and copies both to the root
make test     # runs all tests, interpreted
```

`out/ulang.sh` runs `out/ulang.bin` with a large stack, which the interpreter and the compiler need, so run the commands by it.

## Running

```
out/ulang.sh run file.u -- args           # checks a program and calls its procedure main
out/ulang.sh check file.u                 # checks it: errors of parsing, of types, and of format
out/ulang.sh check --test --prove file.u  # also runs its tests and proves its lemmas
out/ulang.sh format file.u                # rewrites it in the layout that check requires
out/ulang.sh compile file.u -o file.c     # compiles it to C, whose main calls the procedure main
cc -I ulang/runtime file.c ulang/runtime/ulang.c -lgmp -lgc
out/ulang.sh refactor @show file.u:main   # edits and queries by declaration, by scripts of actions
out/ulang.sh help                         # lists the commands
```

## Documentation

- `doc/language.md`: the language, by example, with the principles of its design and the habits of other languages that fail here.
- `doc/tool.md`: every command of `ulang.sh`, `run`, `check`, `hints`, `format`, `compile`, and `refactor`, with its options and replies, and the targets of the Makefile.
- `doc/implementation.md`: the phases and packages of the implementation, the data between them, and the invariants that cross them; `doc/implementation-notes.md` has the detail of each phase.
- `CLAUDE.md`: the rules of working in this repository, the conventions of tests, and the style of the sources.
- `ideas/`: the open work and the plans, listed in `ideas/index.md`.
- `vscode/`: an extension for VS Code and VSCodium, installed by `make install`.
