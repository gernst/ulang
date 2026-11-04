# Overloading across modules

A plan, not implemented yet. Several visible modules may declare the same name, e.g. `length` in `list`, `set`, and `map`. Each use resolves statically, by its type, to exactly one of these declarations, and the checked program records that declaration by its qualified name.

## Scope

- **Across modules only**: overloads are declarations of the same name in different modules, so their qualified names differ already (`list.length`, `set.length`). The interpreters and the compiler key functions by qualified name and arity, and need no change.
- **Within a module**, overloading stays by arity only. Overloading by type there would need mangled names in `Run.scala`, `self/run.u`, and `self/compile.u`.
- **No type classes**: a generic `function f[C](c: C) := length(c)` remains impossible, since declarations have explicit signatures and no constraint enters a type scheme. Neither does the plan change `==` and `compare` of sets and maps, which compare their trees, also inside generic code such as `map` keyed by sets. That needs canonical trees or a builtin that compares elements, independently of overloading.
- **DESIGN.md**: principle 1 excludes overloading by type. Reword it to overloading across modules, resolved statically, so that every use still names one declaration.

## Candidates

`candidates` in `Check.scala` takes a declaration of the current module alone and otherwise those of the visible modules, and `ambiguous` rejects more than one. Instead, the candidates are the declarations of the current module and of all visible modules together, functions, constructors, procedures, and builtins as in `lookup`, and the types decide among them. If several still fit at the end of the command, a single one of the current module is taken, as when it hid the others, e.g. the own `helper` of `tests/modules.u` next to those of its imports. Local variables still hide all of them. A qualified name `set.length` has only the one candidate, as now.

## Resolution

1. **Directly, where possible.** For an applied name with several candidates, infer the arguments first, then keep the candidates of the arity whose instance unifies with the types of the arguments and the expected type. Each candidate is tried on a copy of the substitution, which is cheap since it is a value: `su` in a `Ref` in Scala, and `State` in `self/check.u`. One remaining candidate is committed, by unifying again on the real substitution. None is an error that lists the candidates with their types.
2. **Deferred, otherwise.** With several remaining candidates, the use gets a fresh type and a placeholder name, and the checker state records a pending entry: the position, the name, the remaining candidates, and the type.
3. **Solved to a fixpoint** at the end of each command, before `zonk`: each round drops the candidates of every entry that no longer unify, and commits entries with one candidate, until no round changes anything. An entry without candidates is an error, and one with several is ambiguous, both at the position of the use with the candidates and their types.
4. **Placeholders are replaced** by `zonk`, which walks the checked command at its end anyway.
5. **Lets** do not generalize the types of pending entries: they are solved first, before `generalize`, and what stays pending remains monomorphic.

## Fields

The same mechanism resolves accesses `e.f` and updates `e with f := a` (TODO, Fields 1): an access whose type of `e` is not known yet becomes a pending entry, whose candidates are the accessors `T.f` of the visible datatypes, so that `lambda p => p.x` is checked once `p` is used. This also removes the error that the type of the value of `.f` is not known.

## Steps

1. **Direct resolution** in `Check.scala` and `self/check.u` (`lookup`, `resolve`, `resolve_name`), with tests in `tests/` and the errors of no and several candidates in `tests/errors/`, then `make bootstrap`.
2. **Deferral**: pending entries, the fixpoint, placeholders in `zonk`, and lets, in both checkers.
3. **Fields** as pending entries, in both checkers.
4. **The library**: one name per operation across `list`, `set`, and `map`, and their uses in `self/`: `length` for `list.length`, `set.count`, and `map.size`; `contains` for `list.contains` and `set.member`; `has_key` for `list.has_key` and `map.has`; `get` for `list.lookup` and `map.get`; `remove` for `list.remove` and `map.delete`; `empty` for `set.empty_set` and `map.empty`; `put` for `set.insert` and `map.put`. The helpers that are named apart against clashes, e.g. `snode` and `mnode` or `depth` and `height`, may then share names too. Then `{}` as an `empty()` resolved by the expected type, and the literals of sets and maps resolved through the imports of the module (TODO, Fields 3).
5. **TODO.md and DESIGN.md**: remove Syntax 4 and Fields 1 and 3 from TODO.md as they are done, and reword principle 1 of DESIGN.md.
