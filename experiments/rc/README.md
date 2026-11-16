# Reference counting in the C backend, an archived experiment

This branch, `rc`, tries reference counting with reuse in place (Perceus of Koka, λRC of Lean 4) for the C backend, as
Compiler 12 of `ideas/compiler.md` on `main` proposes. It is archived, not merged: counting is correct and pays off on
small programs, but the way it is built here, as a rewrite of the generated C, cannot be made free of leaks with
confidence. The conclusion and the recommended design are at the end. Measurements are from 16 and 17 November 2026,
on a shared machine at a load of about 3, so times are rough.

## What the branch contains

- `ulang/compile/rc.u`: a pass over the `CFile` that `compile_program` gives, run when the environment sets
  `ULANG_RC` (`count_refs`, hooked into `compile_prog` of `ulang/compile/compile.u`). Per C function it
  1. renames variables declared again in sibling blocks, so that the analysis may go by names (`uniq_stmts`);
  2. replaces aliases `u_value s = v`, such as the scrutinee of a match, by the variable (`aliased`);
  3. nests the flat chains of cases `if (!t && test) {…}` into `else`s where each case sets its flag, so that a
     case body does not seem to keep the scrutinee alive (`chained`);
  4. classifies the value variables as owned or borrowed, the borrowed field variables of a match as children of
     the owned variable they are read from, and what each borrowed variable keeps alive (`classified`);
  5. infers for every compiled function and procedure which formals it takes over, a fixpoint from all borrowed:
     a formal is owned if it is passed to an owned place, returned, stored, or taken apart where a construction of at
     least as many fields may reuse its cell (`borrow_modes`, `takes_over`);
  6. computes liveness, and rewrites forward with the set of variables that definitely own a reference: a variable
     is moved at its last use, copied by `u_dup` elsewhere, dropped where it dies, and, where it dies while its
     children live or a construction follows, claimed by `u_claim`, whose cell the next construction reuses by
     `u_reuse_*`; `u_update_own` updates a record in place; a `return` drops what the result only read; unused
     claimed cells are freed at the end of their scope; and results that nobody takes over become temporaries that
     are dropped after their statement (`count_stmts`, `count_stmt`, `count_expr`, `release`, `hoisted_stmt`).
- The runtime, `ulang/runtime/ulang.c` and `ulang.h`: the count in the 4 bytes of padding after `size` of compound
  objects (so no layout changes, also for the x86 backend), the plain types in a linker section `u_plain`, static
  objects marked `U_IMMORTAL`, `u_dup`, `u_drop` (which drops the items of a dead cell and puts it on the free list
  of its size), `u_claim`, `u_reuse_*`, `u_update_own`/`u_update_rc`, `u_free_cell`, the runtime's own copies of
  references counted where `u_rc` is set (which only the counted `main` sets, so x86 programs are unchanged), and
  the folds of arrays and strings releasing their accumulators. Builds: `-DULANG_RC_CHECK` aborts on any use of a
  dead cell and fills its items with a poison object; `-DULANG_RC_STATS` prints counters at exit.
- `experiments/rc/`: the sample programs and the tools of the measurements below.
  - `bench.u`: 300,000 map inserts and 100 rebuilds of a list of 6,760 records with `with`.
  - `leaks.u`: one procedure per pattern, chosen by an argument: `folds`, `maps`, `gets`, `buffers`, `tries`.
  - `tokloop.u`: the token loop of the lexer in small; `lexing.u`: lexes its arguments with `lex_all` of the
    project, to be copied below `out/` of the root so that it may import the project.
  - `garbage.sh <program.c> <args…>`: builds a debug runtime with the collector disabled during the run, a
    finalizer on every 64th (or every `RATE`th) compound allocation, removed when the counts free it, and one
    collection at exit, and prints the cells that nothing reached but the counts never freed, by the function and
    line of the C that allocated them.
  - `peak.py <label> <command…>`: runs a command with `GC_DONT_GC=1` and prints its CPU time and peak memory.

To try it: `make`, then `ULANG_RC=1 out/ulang.sh compile file.u -o file.c`, then compile as usual, adding
`-DULANG_RC_CHECK` or `-DULANG_RC_STATS` if wanted; run without the collector by `GC_DONT_GC=1`.

## Findings

### Correctness

- The counted compiler compiles itself to the same C as the uncounted one, also in the check build, which aborts on
  any dup, drop, claim, or read of a cell whose count reached zero. Counts are never too low in what was tested.
- Bugs that the check build and the comparison of the C found during the work, each fixed: pointers taken for values
  (`outs[i]` of procedure wrappers); variables of a block that raises at its end released after it; variables that
  the test of a case with alternatives assigns inside the test expression, by `match_expr`, which the pass did not
  see as assignments and dropped (they borrow from the scrutinee); hooks `compare`, `==`, and `show` that the runtime
  calls with borrowed arguments while they own their formals (fixed by wrappers `hb_`).
- Recycling dead cells on the runtime's free lists crashes while Boehm also collects (`-O2` only, depending on the
  layout of the code), while recycling by `GC_free`, or with the collector disabled, compiles the compiler to the
  same C. The counts are right; mixing our free lists with Boehm's collections is not, and was not pursued, since the
  aim is to run without Boehm.

### Reuse and borrowing

- Without borrow inference every call of a read-only function such as `height(l)` cost a dup, a drop, and a claim:
  the compiler compiling itself made 294 million dups and 80 million claims, of which 11 % found a unique cell, and
  ran 25 % slower than uncounted. Inferring borrowed formals cut the benchmark from 67.8 to 30.4 million dups,
  raised its unique claims from 17 % to 97 %, and its time from 0.93 to 0.56 s (uncounted 0.60 s).
- Reuse needs the construction in the same function as the match: `put` of `ulang/prelude/map.u` takes a node apart
  but `balance` and `node` build the new one, so the claimed cell is wasted. Inlining such small builders, or
  passing the cell on, would be needed.
- A formal counts as taken over for reuse only if the body builds a cell of more fields than it reads of the formal;
  without that rule `map.get`, which builds a `Some`, owned its map and made 31 million failing claims.
- With the collector (counts but no freeing), the compiler compiling itself marked a third less, 1.14 s against
  1.71 s, in 12 collections instead of 14, at about the CPU time of the uncounted build: what counting saved in
  marking it spent in counting.

### Freeing and leaks, without Boehm

- The benchmark is exact: all 5,467,808 compound cells are freed, 0.50 s against 0.60 s uncounted with the collector,
  at a peak of 35 MB against 644 MB uncounted without it. The patterns of `leaks.u` leak nothing either.
- The compiler compiling itself leaks: at the end, 17 % of the sampled cells (101,903 of 607,082) were unreachable but
  never freed; its peak was 1.6 GB against 3.5 GB uncounted, both without the collector, and its CPU time 3.4 s
  against 2.8 s. Leaks found and fixed on the way, from 93 % down: the free lists of the wrong size, claimed cells
  that no construction took, a `return` that left the variables it only read, results passed to places that do not
  take them over, closures without captures (size 0, which the first rule took for static), the folds of the
  runtime, the saved slots of a `try` after its body completed, and variables of `NULL` that the joins of paths
  forgot. The last fix, keeping variables known to hold `NULL` owned (`nulls` of `St`), fixed the copy at the
  write-back of `lex_token` but made lexing `if x then y end` leak 29 cells instead of 5; it is the state of this
  commit and the first thing to look at if the work resumes.
- The compiler raises no exception while compiling itself, so the leaks above are not those of exceptions. But a
  `longjmp` skips the drops of every frame it unwinds, so exceptions leak in principle.
- Tracking a single cell (the debug runtime of `garbage.sh` extended by a watch of one allocation, which logs each dup,
  drop, and claim with its caller) was what found the causes; the sampler alone only says where a leaked cell was
  allocated, not where a release was missed.

### Refs

An approximate count over the sources (Python, by indentation, so overloads merge by name and any mention counts as a
read): of 1,789 ref arguments of calls in `ulang/`, 7 % are locals in no `try`, 42 % ref formals whose callers catch
nothing, 1 % in a `try` whose handler does not read them, and 48 % ref formals that some caller's handler may read
restored, from only 25 call sites, the recovery of the parser (`toks`), the per-command recovery of the checker
(`s`), and the `try` of the interpreter. So moving refs into their callees for reuse would not help the state that
those thread. The prototype copies refs at entry, which keeps every value of a ref shared during its procedure.

## Conclusion

Making counts never too low is local and testable: freed memory is reused at once, and the check build aborts at the
first use of a dead cell. Making them never too high is global, and silent: a missing release shows only to a tracing
collector, and Boehm, scanning the stack conservatively, hides some leaks; a leak is reported where its cell was
allocated, far from the missed release; and one missed count on a list cell pins the rest of the list, so one bug
looks like thousands of leaks. The pass rebuilds ownership from the conventions of the generated C, its names, the
shapes of matches, comprehensions, handlers, caches, and wrappers, and a table of builtins, and wherever it is unsure
it copies or keeps, which is safe but leaks; nothing tells when the list of cases is complete. Its must-analysis of
ownership loses precision at every join of paths and every loop, and the refinements interact.

Perceus and λRC avoid this by inserting dups and drops on a small intermediate language whose bindings and uses are
explicit, where freeing every value as soon as it is dead follows from the algorithm, given ownership signatures for
the builtins. For ulang that would be: a pass that inserts ownership on the expanded checked program, or on an IR of
its own between it and the backends, shared by C and x86; one declared table of the runtime, owned or borrowed per
argument and fresh or borrowed per result; exceptions as return codes; strings, big integers, and GMP's limbs counted
too; and the leak oracle of `garbage.sh` over the test suite, so that a leak is a failing test. This branch shows what
that would gain, 35 MB instead of 644 MB and a little faster on the benchmark, and serves as a reference for it.
