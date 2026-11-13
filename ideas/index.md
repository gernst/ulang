# Ideas

Open items of work, each with the problem and the desired solution, one file per area, and the plans of work in progress. An item is named by its area and number, so that Build 1 is item 1 of `ideas/build.md`; within its file, an item names another as item N.

The areas are in the order of their value for the agents that write ulang, which have almost no ulang in their training data and learn the language from its documentation and its errors, while its design, one way of each construct, a canonical format, exhaustive matches, pure functions, and atomic effects, already suits them: so the feedback of a run comes first, the text of what does not parse (Diagnostics 1), the fix (Diagnostics 2), and the resolution of names (Diagnostics 3), then a reference of the language (Documentation 1).

- `ideas/diagnostics.md`: diagnostics and refactorings
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
- `ideas/review.md`: a review of the sources against the rules of style, as of c319a1d
- `ideas/archives.md`: packages compiled separately into static archives (Build 2)
- `ideas/assembly.md`: the backend of x86-64 assembly (Compiler 2)
- `ideas/refactor.md`: the command `refactor`, its edit scripts and operations (Diagnostics 3 and 4)
- `ideas/tester.md`: the tests that the tools generate and the running of tests, as the package `ulang/test` (Testing 1 and 2)
