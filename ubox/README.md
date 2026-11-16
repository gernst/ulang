# ubox

A few commands of Unix in one program, in ulang: `ubox COMMAND [ARGUMENT...]`. 860 lines in 5 files.

```
base64 [-d] [-w COLUMNS] [FILE]     head [-n COUNT] [FILE...]     seq [FIRST [STEP]] LAST
basename PATH...                    ls [-aF] [PATH...]            sort [-rnu] [FILE...]
cat [FILE...]                       mv SOURCE TARGET              tac [FILE...]
cp SOURCE TARGET                    nl [FILE...]                  tail [-n COUNT] [FILE...]
cut [-d CHAR] -f LIST [FILE...]     printenv NAME...              tee FILE...
date                                pwd                           tr SET1 SET2 | -d SET
dirname PATH...                     rev [FILE...]                 true
echo [-n] [TEXT...]                 rm [-rf] PATH...              uniq [-c] [FILE...]
factor NUMBER...                    false                         wc [-lwc] [FILE...]
find [PATH...]                      grep [-ivnc] TEXT [FILE...]
```

Each prints what the command of GNU prints for these options, which `test/compare.sh` checks in 60 cases. Differences that remain: `grep` looks for a text, not a regular expression; `find` lists the files below its paths and takes no tests; `date` prints UTC; `sort` orders by code points; a command reads its whole input before it prints, so none works on an endless stream.

| File | What it holds |
|---|---|
| `box/args.u` | options, `-rn`, `-n 5`, `-n5`, `--`, and `Failure` |
| `box/text.u` | the filters as functions of lines: matching, sorting, runs, fields, sets of `tr`, counts |
| `box/encode.u` | sequences, prime factors, base64, the date |
| `ubox.u` | the commands as procedures, their table, and `main` |

From the root of the repository, after `make`:

```
out/ulang.sh check --test --lint ubox                          # 49 tests and examples, 1.3 s
ubox/test/build.sh                                             # out/ubox/ubox.bin and out/ubox/ubox.sh
ubox/test/compare.sh out/ubox/ubox.sh                          # against the commands of the system
VERBOSE=1 ONLY='^wc' ubox/test/compare.sh out/ubox/ubox.sh     # some cases, with their times
```

ubox was written by an agent on 16 November 2026, every `.u` file through `ulang.sh refactor`, as a test of how well ulang suits agents; what it found is in the items of `ideas/` that cite ubox.
