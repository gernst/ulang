#!/bin/sh
# the .u files below the paths, each compiled by ulang.bin with the entry test and by cc with the runtime object, and run in its
# directory, where a file must succeed, or fail with the error that its first line states, e.g. // type error: unknown variable x,
# as ./ulang.bin test does for directories with the interpreter, which prints the files that do not and N of M files passed
#
# usage: testc.sh <out> <runtime.o> <path> ...
# where out is a directory for the C files, binaries, and results, and the environment may set CC, LIBS, and JOBS

set -u
ulimit -s unlimited

CC=${CC:-cc}
LIBS=${LIBS:-"-lgmp -lgc"}
JOBS=${JOBS:-$(nproc 2>/dev/null || echo 2)}
root=$(pwd)

# testc.sh --one <out> <runtime.o> <file> writes the result of one file to its result file in out, pass or the message
if [ "${1:-}" = "--one" ]; then
  out=$2 runtime=$3 file=$4
  name=$(echo "$file" | sed 's|/|_|g; s|\.u$||')
  c=$out/$name.c bin=$out/$name log=$out/$name.log

  expected=$(head -n 1 "$file" | sed -n 's|^// \([a-z]* error: .*\)$|\1|p')
  [ -n "$expected" ] || expected=success

  if ! ./ulang.bin compile "$file" test "$c" > "$log" 2>&1; then
    got=$(tail -n 1 "$log" | sed "s|^$file: ||")
  elif ! $CC -O2 -w -I self/runtime "$c" "$runtime" $LIBS -o "$bin" > "$log" 2>&1; then
    got="cc failed: $(head -n 1 "$log")"
  elif (cd "$(dirname "$file")" && "$root/$bin" > /dev/null 2> "$root/$log"); then
    got=success
  else
    got=$(grep '^runtime error: ' "$log" | tail -n 1)
    [ -n "$got" ] || got="failed without a runtime error"
  fi

  if [ "$got" = "$expected" ]; then
    echo pass > "$out/$name.result"
  else
    echo "$file: expected $expected, got $got" > "$out/$name.result"
  fi
  exit 0
fi

out=$1 runtime=$2
shift 2
mkdir -p "$out"
rm -f "$out"/*.result

files=$(find "$@" -name '*.u' | sort)

echo "$files" | xargs -P "$JOBS" -I {} sh "$0" --one "$out" "$runtime" {}

total=0 passed=0
for f in $files; do
  total=$((total + 1))
  result=$(cat "$out/$(echo "$f" | sed 's|/|_|g; s|\.u$||').result" 2>/dev/null || echo "$f: no result")
  if [ "$result" = pass ]; then
    passed=$((passed + 1))
  else
    echo "$result"
  fi
done

echo "$passed of $total files passed, compiled"
[ "$passed" -eq "$total" ]
