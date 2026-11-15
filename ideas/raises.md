# Raises

The plan of raises as a datatype of their own, solved by the raises pass from constraints that inference records, with subtyping of raises. Deleted when its work is done; what stays true goes into `CLAUDE.md` and `README.md`, under Semantics.

## 1. The problems

- **Raises are a type**: the field `raised` of `TProc` and of `EProc` of `ulang/syntax/syntax.u` is a `Type`, so that an unknown of inference can stand for a whole set, and a known set is a `TTuple` of its types, sorted and without duplicates, so that structural equality is set equality. But the empty set is then the unit type, nothing in the type says that it is a set, every place that builds one must keep the invariant, and the printers and readers must know from the position that the tuple is a set (`raised_names` of `ulang/check/raises.u`, `normalized_raises` of `ulang/check/check.u`, `raised_text` of `ulang/format/format.u`).
- **Raises unify exactly**: a local of type `(in Int) raises A` does not fit a formal `(in Int) raises A, B`, and a lambda bound by a `let` without a type is `UntypedRaise`, since nothing fixes the unknown of its raises, which the raises pass then reads as raising nothing.

## 2. The representation

`Raises` in `ulang/syntax/syntax.u`, the field `raised` of `TProc` and `EProc`:

- `RWritten(types)`: the types after `raises` as the source writes them, which the parser gives and `format` prints;
- `RSet(names)`: the exceptions by name, the set of the checked program, since an exception is a datatype without type parameters, to which the checker normalizes `RWritten`, reporting `NotAnException` for the others;
- `RUnknown(id)`: an unknown of inference, of the instance of a procedure used as a value, as the param `(raised)` of `proc_scheme` gave, and of a lambda, whose `EProc` has `RUnknown(0)` until the checker gives it an unknown of its own.

The raised types of `CProc`, `CProcSig`, and `ProcSig` stay a `List[Type]`, as written and checked, which `check_signatures` compares.

## 3. Constraints

- Unknowns of raises are numbered across the package, by a counter of the state that `enter` does not reset, unlike the metas of types, since their bounds are solved after the package.
- `unify` of two raises compares two sets at once, `CannotUnify` as now, and records a bound for each other pair, `Bound(module, pos, sub, sup)` at `s.at`, both ways for equality. Raises never bind, so `zonk` leaves the unknowns in the checked program, and they need no substitution and no occurs check.
- The bounds of a command whose check reports errors are dropped, as the raises pass skips its command; the others reach `raises_errors` beside the commands.
- **Subtyping**: `expect` records `found ⊆ expected`, by a polarity through `unify`, flipped for the ins of a `TProc` and for the arguments of a `TFun`, both ways for refs, and the same for the arguments of a `TSort` and the elements of a `TTuple`, since all values are immutable. Calls check their arguments by `expect`, so a procedure value of a fixed type fits a formal that allows more, as a declared procedure and a lambda already do.

## 4. The raises pass (`ulang/check/raises.u`)

- The lower bounds of an unknown are the raises of the declared procedures used as values of its type, the raises of the bodies of the lambdas of it, and the bounds of the checker whose `sup` it is; its solution is the union of them, each exception with the origin where it entered, the use of the procedure, the raise in the lambda, or the bound.
- The fixpoint of `infer_procs` grows the solutions together with the raises of the inferred procedures, since a call through a local raises the solution of its type, and a value raises what its procedure raises. Both only grow, so it ends.
- Then each upper bound, a bound whose `sup` is a set, is checked: an exception of the solution of its `sub` that the set omits is `UntypedRaise` at its origin, as now for a value or a lambda, and at the bound for one that a fixed type brought in.
- Open: the checked program keeps the unknowns, which the backends and the prover ignore, so the hints show a local of a procedure type without its raises. Writing the solutions into it, by `with_inferred`, needs the zonk walk to take them beside the substitution of types, which 40 calls of `zonk_type` of `ulang/check/check.u` pass.

## 5. Steps

Each step checks and tests its files as `CLAUDE.md` says, and is committed on the branch `raises`.

1. `Raises`, the bounds of equality, and their solving: the `let` of a lambda without a type checks now.
2. Subtyping by the polarity of `expect`.
3. Tests in `ulang/test/statements/statements.u`, cases of `ulang/test/errors/types.u` changed or added, `make test`, and `make testerrors` after asking; then `CLAUDE.md` and `README.md`, where "which unification compares exactly, without subtyping" goes, and Checker 4 is narrowed to the params of raises.

## 6. Later

Params of raises, `(in A) raises E` instantiated per use, stay Checker 4: no code of the project needs them, since each higher-order procedure here raises the one exception of its phase, and loops with `try` take the place of `each` and `retry`. The bounds of this plan carry them without rows when they come, designed together with `uses os` (Later 1).
