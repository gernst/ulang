# Exports

Implemented in `self/`, with `set` and `map` as abstract types and export sections for the modules of `self/`, on top of the overloading across modules of `overloading.md`, which `self/check.u` implements. A module consists of an export section and an implementation section. The export section lists the signatures that importers see, and the implementation section defines them along with private helpers and tests, so the top of a file is the interface of the module.

## Layout

```
// sets as AVL trees, ordered by compare, where the heights of the subtrees of each node differ by at most one

export

type Set[A]

function empty[A](): Set[A]
function contains[A](x: A, s: Set[A]): Bool
function insert[A](x: A, s: Set[A]): Set[A]
function from_list[A](xs: List[A]): Set[A]
function elements[A](s: Set[A]): List[A]
function size[A](s: Set[A]): Int

// sets are ordered and equal by their elements, whatever the shapes of their trees
function compare[A](a: Set[A], b: Set[A]): Int
function ==[A](a: Set[A], b: Set[A]): Bool

implementation

data Set[A] :=
  | Leaf
  | Node(Set[A], A, Set[A], Int)

function empty[A](): Set[A] :=
  Leaf

function compare[A](a: Set[A], b: Set[A]): Int :=
  compare(elements(a), elements(b))

function ==[A](a: Set[A], b: Set[A]): Bool :=
  elements(a) == elements(b)

function height[A](s: Set[A]): Int :=
  ...

test set_equality() :=
  ...
```

1. **Imports** come first, before `export`, since the signatures may use their types. They are not exported, as now, since imports are not transitive.
2. **`export`** starts the export section, which contains only:
   - notations, which are always public and allowed only here, so that every notation of a module applies to all of its declarations; a module cannot have private notations,
   - `data T[A] := ...`, which exports the datatype with its constructors and the accessors and updates of its fields, and is defined here, not repeated in the implementation,
   - `type T[A] := U`, an alias, which is likewise defined here,
   - `type T[A]` without a definition, an abstract type, which the implementation defines by `data`, see below,
   - `function f[A](x: A, ...): B` and `procedure p[A](in x: A, out y: B, ref z: C)`, signatures without `:=` and a body.
3. **`implementation`** starts the implementation section: the definitions of the exported functions, procedures, and abstract types, private declarations of any kind except notations and imports, and the tests.
4. **Without `export`** there are no sections, and everything is public, so the prelude, existing files, and programs given to `run` and `test` stay valid, and the migration can go module by module.

## Decided: repeated signatures

Every exported function and procedure is written twice, as a signature and as a definition that repeats it exactly. Every change to an exported signature is two edits, but exported signatures change rarely, the checker rules out drift, and both places stay readable on their own: the export section is the whole interface, and each definition shows its types where it is written. Rejected alternatives:

- **Definitions without types**, `function contains(x, s) := ...`, taking the types from the signature, which hides them from the code that uses them.
- **Marks instead of sections**, `export` or `private` before each declaration, which scatters the interface through the file.

## Meaning

- **Definitions match their signatures exactly**: every exported function and procedure has exactly one definition in the implementation with the same type parameters, formals, modes, and types, where the names of the type parameters and formals must be equal too, so the export section cannot drift from the code. Since overloading within a module is by arity only, a signature determines its definition by name and arity. Missing definitions, mismatches, and repeated signatures are errors at the signature. Since private declarations are hidden by name, a function exported with some of its numbers of arguments must be exported with all of them.
- **Signatures mention only what importers can name**: the sorts declared in the export section, those of imported modules, and builtin types. This is checked in the export section alone, before the implementation, which also rules out private types in public signatures.
- **Candidates**: in other modules, the private declarations of a module are not candidates of overloading, so `height`, `node`, or `balance` of `set` and `map` no longer meet in a module that imports both. Within the module, everything is visible, including the preference for a single declaration of the module itself. Exports narrow the candidates but do not replace overloading: two exported declarations of one name and type in two imports still need a qualified name.
- **Qualified names** `m.x` reject declarations that `m` does not export. Such names still reach every module of the program (TODO, Fields and literals 2).
- **Literals of sets and maps**: the parser translates them to the qualified names `set.from_list`, `set.elements`, `map.put_all`, `map.empty`, and `map.entries`, which `set` and `map` must therefore export, or literals and comprehensions break once these modules have export sections. Resolving literals through the imports of the module (TODO, Fields and literals 2) is independent of this plan, but should respect exports in the same way.
- **Errors**: a name whose declarations in other modules are all private reports `x is private to module m`, but only when no public declaration of the name is visible at all. Otherwise private declarations are ignored entirely, so that one which happens to fit does not change the message that no overload fits.
- **Run time**: the interpreter and the compiler key declarations by qualified name already, so `self/run.u` and `self/compile.u` only skip the new commands, and they compare abstract types through the synthesized functions, see Orders and equalities, like declared ones.

## Orders and equalities

A function `compare` or `==` of two values of a datatype of its own module, see `order_of` in `self/check.u`, is the order or equality of that datatype, which the builtins use for its values everywhere, also inside other values and in generic code. It therefore belongs to the interface of the datatype:

- **Only abstract types declare equalities.** A datatype whose constructors are visible, exported by `data`, private in the implementation, or of a module without `export`, has the structural equality, since matching tells its constructors apart anyway, so that equal values would otherwise behave differently. A `==` of two of its values is an error at the declaration, rather than an ordinary function that the builtins ignore. Types whose values are equal in other ways, such as fractions, are abstract.
- **Datatypes with visible constructors may declare `compare`**, which rearranges their values, e.g. dates by time or versions by number, rather than by the positions of constructors and fields, and which `<`, `<=`, `>`, and `>=` follow. It must agree with the structural equality: `compare(a, b) == 0` exactly when `a == b`, so that it does not merge values that `==` tells apart, which would make `contains` of a list and of a set disagree and merge keys of maps. Neither the checker nor the run time checks this, since a structural comparison after every `0` would cost too much, so it is the obligation of the module, like the totality of `compare`. It is exported like the orders of abstract types.
- **Exported types export their declared `compare` and `==`** in the export section, otherwise they are an error at the definition, since they change the meaning of `==`, `compare`, `contains`, and the keys of maps and sets for importers, who should see this at the top of the module.
- **Missing orders and equalities of abstract types fail at run time**, by functions that the checker synthesizes, in place of `complete_orders`:
  - **`compare` and `==` declared**: both are used.
  - **`compare` alone**: `==` is `compare(a, b) == 0`, as the builtins do already for types with a declared order (`u_equals_by_compare` in the runtime, and the case `(None, Some(_))` of `equal` in `self/run.u`), so nothing is synthesized.
  - **`==` alone**: the type is unordered: the checker synthesizes `compare`, which raises `Unordered("T")`. `==`, `!=`, and `contains` of lists work, while `compare`, `<`, and everything built on them, such as the keys of maps and the elements of sets, raise. This replaces the static error `EqualityWithoutOrder` of `complete_orders`.
  - **Neither**: the type is incomparable: the checker synthesizes both, raising `Incomparable("T")`, so that not even `==` works.

  The synthesized functions are named `T.compare` and `T.==`, like the accessors `T.f`, so that two abstract types of one module do not clash, since overloading within a module is by arity only, and they have the form that `order_of` recognizes, which takes these names for `compare` and `==`, e.g. `function Set.compare[A](a: Set[A], b: Set[A]): Int := raise Unordered("set.Set")`, so the interpreter and the compiler treat them as declared orders, and the builtins reach them for the values everywhere, also inside other values, e.g. lists of them, in generic code, and in the module itself, since the order belongs to the type, not to the module that compares. They are private, so importers reach them only through the builtins, and the rule that exported types export their orders applies only to declared ones. So the language neither forces an order on abstract types nor assumes that their structure is one, which would let importers tell apart values that the module considers equal, e.g. two trees of the same set. As for functions and procedures, which already fail with `cannot compare functions`, the checker does not see this, since `==` and `compare` take values of any type, and a static check would need constraints on type parameters, i.e. type classes, which are out of scope.
- **`Unordered(Str)` and `Incomparable(Str)`** are new exceptions of the prelude, next to `MissingField(Str)`, with the qualified name of the type, which a program can catch. They tell apart a type without an order, whose values can still be tested for equality, from one whose values cannot be compared at all.
- **`!=`** need not be exported, since `prelude.!=` uses the builtin `==`, which takes the declared or synthesized equality at run time.
- **`<` follows `compare`**: the builtin `<` is generic, `[A](A, A) -> Bool`, with the value `compare(a, b) < 0`, and `<=`, `>`, and `>=` of the prelude are generic over it, each by one `<`: `x <= y` is `not (y < x)`, `x > y` is `y < x`, and `x >= y` is `not (x < y)`, which is correct since `compare` is a total order. Each comparison then costs one `compare`, not a `compare` and an `==` as `m < n or m == n` would, and all four depend only on `compare`, never on a declared `==`. They stay prelude functions rather than builtins, which would cost three registrations each (TODO, Builtins 1), for the saving of one call. So a declared order applies to the comparison operators too, also on abstract types, without exporting them. Today `<` is a builtin on `Int` only, in `self/check.u`, `self/run.u`, and `u_lt` of the runtime, which calls `compare_ints`, and the prelude defines the other three on `Int`. Two consequences:
  - `<` then accepts functions and procedures, which fail at run time with `cannot compare functions`, as `compare` and `==` do already.
  - Integers stay fast: `u_lt` and the case of `<` in `self/run.u` keep a direct path for two integers before the generic `compare`, which dispatches through the type descriptor, so that loop bounds do not pay for the dispatch.

## Abstract types

`type T[A]` in the export section exports the sort `T` with its arity, but not its constructors or fields. This is the additional infrastructure:

1. **A sort without a definition**: a new command, see Representation, which declares `T` with its number of parameters when the export section is checked, so that the signatures can use it.
2. **The definition in the implementation** is a `data T[A] := ...` with the same parameters. A missing or extra definition, or a different arity, is an error. An alias `type T[A] := U` is not allowed as the definition, since importers would then need the checker to stop expanding it outside the module, so `T` stays a nominal type.
3. **Hiding the members**: the constructors of `T` and the accessors `T.f` and updates `T.f:=` of its fields go into `hidden`, while `T` itself stays a sort, so it resolves everywhere. In other modules this has the following effects, and within the module `T` is an ordinary datatype:
   - **Names**: constructors in expressions and patterns are found by `overloads`, which skips hidden names of other modules, also for qualified names, so `Leaf` of `set` and `map` stop being overloads for importers, and `set.Leaf` reports `Leaf is private to module set`.
   - **Fields**: `field_access` finds `T.f` by `schemes_of` once the type of `e` is known, and otherwise takes the accessors of all datatypes with the field as candidates; both skip hidden accessors, which reports an unknown field.
   - **Exhaustiveness**: `constructors_of`, which lists the constructors of a type for the check of matches, gives `None` for a type whose members are hidden from the current module, as for `Int`. A match on `T` then needs a variable or `_`, and its error messages never show hidden constructors.
4. **Compared through the synthesized functions** where the module declares no `compare` or `==`, see Orders and equalities. The interpreter, the compiler, and the runtime need no change for this, since they find the synthesized functions by `order_of` like declared ones. `os_show` still prints the structure, which serves debugging and does not let a program depend on the representation.

## Representation

- **Commands** in `self/syntax.u`:
  - `CExport` and `CImplementation`, markers like `CImport`, placed where the sections start,
  - `CFunSig(Name, List[Id], List[List[Var]], Type)` and `CProcSig(Name, List[Id], List[Var], List[Var], List[Var])`, the signatures, as `CFun` and `CProc` without their bodies,
  - `CAbstract(Name, List[Id])`, the abstract types.

  The parser keeps the markers in the commands of a `CModule`, which otherwise loses its tests, and parses signatures by the same code as the declarations, up to `:=`.
- **`hidden: Set[Name]` in `Ctx`** (`self/ctx.u`, including `restrict` and `union`): the private declarations, qualified, including the members of abstract types. A set of hidden names rather than exported ones means that a module without `export` has the empty set. `overloads`, `visible_names`, `field_access`, and `constructors_of` consult it when the module of a name is not the current one.

## Steps

Each step ends with `make test`, which picks up new files in `tests/` and `tests/errors/` by itself, `make selftest`, and `make selfcompile`, or `make bootstrap` where the checker inside `ulang.bin` must know a construct before the compiler's own source uses it.

0. **Generic `<`**, independently of the rest: the type of `<` in `self/check.u`, its evaluation by `compare` in `self/run.u` with the direct path for integers, `u_lt` by the generic `compare` of the runtime with the same path, which takes declared orders through the type descriptor, and `<=`, `>`, and `>=` with a type parameter in the prelude, each by one `<`. Tests in `tests/orders.u` of `<` on strings, tuples, and sets, and on a datatype with a declared order, which is `Fraction` of `tests/orders.u` until step 3 makes it abstract.
1. **Parser**: the keywords `export` and `implementation` in `self/lex.u` and `self/parse.u`; the signatures, abstract types, and markers; and errors for sections out of order or repeated, imports after `export`, notations in the implementation, and definitions in the export section other than `data` and aliases. Tests go in `tests/errors/`.
2. **`self/check.u`**: the export section first (its sorts, aliases, datatypes, and signatures, and that they mention only nameable sorts), then the implementation with the matching of definitions to signatures and the exported orders, then `hidden` in the candidates and qualified names, and the error for private names. Tests: `tests/exports.u` with a module that has a private helper whose name and type clash with one of another import, and errors in `tests/errors/`.
3. **Abstract types** in `self/check.u`, with tests of matching, constructing, and fields from outside in `tests/errors/`, and the synthesized orders and equalities, which replace `complete_orders`, with `Unordered` and `Incomparable` in the prelude. Tests: in `tests/errors/`, `// runtime error: uncaught exception Incomparable("...")` for `==`, `compare`, and `contains` of a list on an abstract type with neither, from outside and inside its module, and `// runtime error: uncaught exception Unordered("...")` for `compare` and a set of an abstract type with `==` alone; `// type error: ...` for a `==` of a datatype with visible constructors, which `equality-without-order.u` turns into; `Fraction` of `tests/orders.u` moved to a module of its own as an abstract type; in `tests/`, that `==` works for an abstract type with `compare` alone and with `==` alone, and that both exceptions can be caught.
   Tests in `tests/orders.u` of a datatype with visible constructors ordered by a declared `compare`, e.g. versions by number, with `<`, the elements of sets, and the keys of maps.
4. **Migration**: export sections for the library modules, with `Set` and `Map` abstract, their `compare` and `==` exported, the functions that literals and comprehensions call exported, and their `!=` removed, since `prelude.!=` reaches their `==` through the builtin, and for `self/`, where `ctx.u` and `c.u` dropped their prefixes against clashes.
5. **Docs**: CLAUDE.md (what a module sees, the sections, `Unordered` and `Incomparable` among the exceptions of the prelude, the synthesized orders, and which types may declare `compare` and `==`), DESIGN.md ("Missing"), TODO.md (the item under "Look at"), and the keywords in the grammar of `vscode/`.
