# Ideas

Open items of work, each with the problem and the desired solution, one file per area, and the plans of work in progress. An item is named by its area and number, so that Build 1 is item 1 of `ideas/build.md`; within its file, an item names another as item N.

The areas are listed by their value for the agents that write ulang, which have almost no ulang in their training data and learn the language from its documentation and its errors, while its design, one way of each construct, a canonical format, exhaustive matches, pure functions, and atomic effects, already suits them. The items of most value, in an order that lets each help the next: the open items of `ideas/refactor.md` that its uses asked for; Compiler 1 and Testing 6; Diagnostics 6 and 3; Syntax 4 and Modules 2; Build 1, first `check --test-compiled`; and the soundness of the prover, Verification 9, then 10, 8, and 11. Decisions without work, proposed by a review of the design (5cd2fe1) and not yet taken: Modules 1 together with Later 1, by capabilities as Later 4 describes; Later 2 dropped, since capabilities give its replay; Testing 7 dropped unless the crash recurs; and Code 5 dropped, its principles added to Style of `CLAUDE.md` with the items that need them.

- `ideas/diagnostics.md`
- `ideas/testing.md`
- `ideas/documentation.md`
- `ideas/checker.md`
- `ideas/syntax.md`
- `ideas/modules.md`
- `ideas/code.md`
- `ideas/library.md`
- `ideas/build.md`
- `ideas/compiler.md`
- `ideas/verification.md`
- `ideas/later.md`
- `ideas/rejected.md`: what was rejected or decided, with the reasons
- `ideas/examples.md`: programs that would show ulang on tasks of real software, in `examples/`
- `ideas/properties.md`: the properties that make code cheap to check, and the items of the other files that give them

Plans of work in progress, each deleted when its work is done:

- `ideas/prove.md`: the prover, its proofs, and contracts
- `ideas/lint.md`: the rules of lints that need the e-graph of the prover
- `ideas/refactor.md`: the open items of the command `refactor`, from its uses and its design, with what working on them needs (Diagnostics 4)
