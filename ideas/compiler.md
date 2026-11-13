# Compiler

1. **Runtime errors have no positions.** Solution: positions in the messages of the runtime and of the interpreter, first for uncaught exceptions and failed `assert`s, from the positions that the checked program keeps (`At` of `ulang/syntax/syntax.u`).
2. **Programs need a C compiler**, and `cc -O2` takes most of the time of `make selfcompile`. Solution: a backend that writes assembly for x86-64, see `ideas/assembly.md`.
3. **No optimizations.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, the rewrites of Diagnostics 5, and loops for folds of comprehensions beyond `list.foldr` into lists, such as those of sets and maps and of several generators.
