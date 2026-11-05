# Containers

Lists, sets, and maps share one interface, and the comprehensions of the parser are folds over it, without intermediate lists. Implemented in part: the folds, comprehensions as folds, and the loop of `self/compile.u` for `list.foldr` into lists. Open: the rest of the interface, literals through it (TODO, Fields and literals 2, and Syntax 3), and loops for the other folds.

## Implemented

- **Folds**: `foldr(c, acc, f)`, which is `f(x1, f(x2, ... f(xn, acc)))`, and `foldl(c, acc, f)`, which is `f(... f(f(acc, x1), x2) ..., xn)`, in `list`, `set`, and `map`, in the order of lists and in ascending order of sets and maps, where `f` of maps takes the key and the value as two arguments. `foldr` of sets and maps visits the right subtree first, so it recurses as deep as the tree is high, and `foldr` of lists as deep as the list is long.
- **Comprehensions are folds** over the containers of their generators, nested for several generators, where each generator sees the variables of those before it, the condition is an `if` in the innermost function, and the accumulator has a name that users cannot write:
  - `[e for x in c if q]` is `foldr(c, Nil, lambda x, acc => if q then Cons(e, acc) else acc)`, which keeps the order,
  - `{e for x in c if q}` is the same with `set.empty()` and `set.insert(e, acc)`,
  - `{k1 => v1 for k => v in m if q}` is `foldl(m, map.empty(), lambda acc, k, v => if q then map.put(k1, v1, acc) else acc)`, so that the last entry of a key counts, as in a literal.

  So generators range over lists, sets, and maps alike, by the fold that overloading chooses for the type of the container, where the module of the container must be imported.
- **Names that locals do not hide**: `foldr` and `foldl` of the translations are names of the module `*`, which users cannot write, and which `overloads` of `self/check.u` resolves against the declarations of the visible modules, but never against locals, so that a local `foldr` does not capture them.
- **A loop for lists**: `self/compile.u` translates `list.foldr(xs, init, lambda x, acc => if q then Cons(e, acc) else acc)`, or with `Cons(e, acc)` alone, where `acc` is free in neither `q` nor `e`, into a loop that appends to the result in order, through a pointer to the tail of its last cell, and puts `init` at its end. This is the loop of the former comprehensions of the core, `EComp`, which is gone from `self/syntax.u`, the checker, `self/run.u`, and the compiler. All list comprehensions of the compiler take this loop.

## Costs

Checking the compiler takes 0.40 instead of 0.36 seconds, and the self-compile 0.56 instead of 0.54, since each comprehension in a module that imports `set` and `map` resolves `foldr` among three candidates by trials. A cheaper resolution would filter the candidates by the sorts of arguments whose types are known before the trials, which helps every overloaded use. Comprehensions of sets and maps, and the outer generators of several, are calls of the folds with closures, without the lists of `set.elements` and `map.entries` that they built before.

## Open

1. **The rest of the interface**: `empty`, `size`, `contains`, `insert`, `remove`, `elements`, `from_list`, and `++` for all three, after the decision on the association lists of `list`, whose `get`, `put`, `remove`, and `has_key` treat a `List[K * V]` by keys while the interface treats a list by its elements, with 72 uses in `self/`: a module `assoc`, or maps in their place.
2. **Literals through the interface**: `{a, b}` as `from_list([a, b])` and `{}` as `empty()`, resolved by hints of their types such as `Set[_]` and through the imports of the module, instead of the qualified names `set.from_list` and `map.put_all` that reach every module of the program (TODO, Fields and literals 2, and Syntax 3).
3. **More loops** in `self/compile.u`: folds of sets and maps with literal lambdas, and nested generators, whose outer folds are calls with closures today (TODO, Self-hosted toolchain 6).
4. **A native `foldr` of lists** in the runtime and `self/run.u`, which applies `f` from the last element to the first without recursion, for folds that are not loops.
