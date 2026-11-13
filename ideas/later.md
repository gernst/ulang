# Later

1. **Procedures may do anything that `os` offers**, invisibly in their signatures. Solution: a clause `uses os` on exported procedures.
2. **Failures of programs that read files or arguments are hard to reproduce.** Solution: `run` records the results of the `os_*` procedures and replays them.
3. **No native code of users.** Solution: `builtin` sections outside the library, with the native definitions linked in by `ulang.bin build` (Build 1).
