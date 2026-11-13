# Later

1. **Procedures may do anything that `os` offers**, invisibly in their signatures. Solution: a clause `uses os` on exported procedures.
2. **Failures of programs that read files or arguments are hard to reproduce.** Solution: `run` records the results of the `os_*` procedures and replays them.
3. **No native code of users**: a `builtin` section outside the library checks, but nothing can define its names. Solution: native definitions of them linked in by `ulang.bin build` (Build 1), and found by the interpreter.
