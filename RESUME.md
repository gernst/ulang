# Resuming: the formatter and lines of at most 80 characters

This file is a note for whoever continues (a person, or an agent such as Claude Code); delete it when the work is done. Open items belong in `TODO.md` (item 8, `format` cannot reformat the code base, and item 10, comments that are not docs).

## Goal

`ulang.bin format` should rewrite the code base without damage (TODO.md item 8) and leave a formatted file unchanged. The printer cannot break lines inside expressions. It breaks displays, heads of procedures, and cases (rules in the last bullet of `CLAUDE.md`, Style), so every other printed line has to fit in 80 characters, which the source achieves by helper functions and locals (`let ... in` does not help, it prints on one line).

## Done and committed (main, up to `c0a09a9`)

- Docs are syntax: `//` runs above modules, commands, constructors, and cases are in the tree and printed back.
- `format.u`, `parse.u`, `lex.u`, and `ulang.u` have helper functions so that their printed lines were at most 120 characters (the work of two agents, merged); `compile.u` too.
- The printer (`self/format.u`):
  - A `list`, `set`, `map`, or `array` display wider than 80 characters has one element per line (`form_text`, `spread_form`).
  - A procedure head wider than 80 characters has one formal per line, with `in  `, `out `, `ref ` before the first formal of a group and `)` on its own line (`proc_head`, `spread_proc_head`, `mode_lines`). Function heads are not broken.
  - A case body starts on the line after `=>`; a blank line separates the cases unless there are at most two and each has a head and one line of body (`case_chain`).
  - Imports are one block, sorted bytewise by their printed lines. `sort` is new in `prelude/list.u`; `same_mod` of `self/ulang.u` compares the imports sorted.
- Calls next to an `assign` in a row of calls and assignments are padded to `call   ` (`runs`, `stmt_line`), a row of calls only is not; `-1` as the right operand of an infix operator is printed without parentheses, as the left operand with them; `for` loops are printed as written (`for_parts`).
- The sources were reformatted by `format` (`857555b`), and `make test`, `make testformat`, and `make testc` pass (reported by the user).
- The worktrees and branches of the agents are removed.

## Not done

Formatting a file of `self/`, `prelude/`, or `tests/` again still changes nearly all of them (1751 changed lines of `check.u`, 956 of `parse.u`, 1 of `tests/counter.u` at `c0a09a9` plus the rules above); compare `format f` with `f` to see which rules are missing besides these.

1. **`local` lists and long literal tables** (`native_procs`, `builtin_functions`, `test_operators`): break like parameter lists, one element per line (proposal, not decided).
2. **Function heads** have no modes: decide whether they break before each parameter.
3. **Blank lines between statements**, and the rest of TODO.md item 8; then `make testformat` should also require that `format` leaves each file unchanged.
4. **Shorten the sources to 80 characters.** The old measure was for 120 characters; change `120` in `long.py` below to `80`. Many files exceed 80: expect the most in `self/check.u`, `self/run.u`, `self/compile.u`, and the library. Do this after the printer is complete.
5. **Smaller:** the message functions of `self/ulang.u` (`type_message` and others) could move to `self/error.u` or a new module, if no import cycle results; review the names of the new helper functions against the naming rules of `CLAUDE.md`; the untracked `doc/`, `examples/`, `.claude/`, and `bootstrap/<time stamp>/` are not decided.

## The measure

Save as `long.py` anywhere, and run from the repository root with a built `./ulang.bin` (`ulimit -s unlimited`): `python3 -I long.py self/check.u v d`. It prints the lines of the printed form over the limit as `[declaration] width: text`; ignore those whose text starts with `procedure`, `function`, or `local`, and literal tables. A `case P => body` counts as fitting if `case P =>` fits and `body` fits on its own line, indented by 4 more, which is how the printer lays out cases.

```python
import sys,re,subprocess,collections
LIMIT=80
f=sys.argv[1]
out=subprocess.run(['./ulang.bin','format',f],capture_output=True,text=True).stdout.split('\n')
cur='';res=[]
for l in out:
    if re.match(r'^(function|procedure|test) ',l): cur=re.split(r'[(\[ ]',l.split(' ',1)[1])[0]
    if l.lstrip().startswith('//'): continue
    w=len(l)
    m=re.match(r'^(\s*)case (.*?) => (.+)$',l)
    if m and not l.rstrip().endswith('=>'):
        w=max(len(m.group(1))+len('case '+m.group(2)+' =>'), len(m.group(1))+4+len(m.group(3)))
    if w>LIMIT: res.append((cur,w,l.strip()[:100]))
c=collections.Counter(x[0] for x in res)
if len(sys.argv)>3:
    for cur_,w_,t_ in res: print(f"[{cur_}] {w_}: {t_}")
print(f, len(res))
if len(sys.argv)>2:
    for k,n in c.most_common(): print(f"  {n:2d} {k}")
```

## How to shorten (what worked in `compile.u`)

- A function: split into small named helpers; a procedure: more `assign` statements with extra `local`s. Behaviour must not change; `make test`, then `make selfcompile` (fixpoint) and `make testc` at the end.
- Never put a helper between a doc comment and the declaration it documents: insert it above the whole comment run or below the declaration. Add no comments.
- Exported names and types stay as they are; `local` must be declared at the start of a body (a `local` after an `assert` is a parse error).
- Repeated idioms became builders (`declare`, `set_var`, `call_c`, `guarded`, `case_if`, ...); long type expressions became aliases (`type Fusion := ...`).

## Lessons

- The compiler `./ulang.bin` is built from `bootstrap/ulang.c`, so a change of `self/format.u` shows in `ulang.bin format` only after `make selfcompile`, which replaces `bootstrap/ulang.c`; to try a change without that, run the interpreter, `./ulang.bin run self/ulang.u main -- format <file>`, which takes minutes for large files.
- Agents in worktrees started from an old commit, not from `HEAD`: the first step in a worktree must be `git reset --hard <sha of main>` and `make`. Do not let agents edit files that someone else edits.
