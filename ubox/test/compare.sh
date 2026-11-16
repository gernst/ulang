#!/bin/sh
# compares ubox with the commands of the system, each case by its output and its status
# usage: [ONLY=pattern] [VERBOSE=1] [LIMIT=seconds] ubox/test/compare.sh "<command that runs ubox>", e.g. ubox/test/compare.sh out/ubox/ubox.sh, from the root
set -u
UBOX=$(cd "$(dirname "${1:?the command that runs ubox}")" && pwd)/$(basename "$1")
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$ROOT/out/ubox/compare
rm -rf "$WORK" && mkdir -p "$WORK/tree/sub" && cd "$WORK"

printf 'pear:3:green\napple:10:red\napple:10:red\nFig:2:purple\n\ncherry:25:red\n' > fruit.txt
printf 'zebra\nÄpfel und Birnen\n  two  words\n' > words.txt
head -c 3000 /dev/urandom > random.bin
printf 'x' > tree/a.txt; printf 'y' > tree/.hidden; printf 'z' > tree/sub/b.txt

# the system's commands in the order of code points, as ubox sorts, and reading UTF-8, without which rev hangs
export LC_ALL=C.UTF-8
pass=0; fail=0
# ONLY is a pattern of grep -E for the names of the cases to run, VERBOSE=1 prints the time of each, LIMIT its seconds
ONLY=${ONLY:-.}; VERBOSE=${VERBOSE:-}; LIMIT=${LIMIT:-10}
selected() { printf '%s' "$1" | grep -Eq "$ONLY"; }
# case NAME 'stdin' COMMAND ARGS...: the same command line for ubox and for the system
case_() {
  name=$1; input=$2; shift 2
  selected "$name" || return 0
  start=$(date +%s%N)
  printf '%s' "$input" | timeout $LIMIT $UBOX "$@" > got.out 2> got.err; got=$?
  mid=$(date +%s%N)
  printf '%s' "$input" | timeout $LIMIT "$@" > want.out 2> want.err; want=$?
  end=$(date +%s%N)
  [ -n "$VERBOSE" ] && printf '%-16s ubox %5d ms, system %5d ms\n' "$name" $(((mid - start) / 1000000)) $(((end - mid) / 1000000))
  [ $got -eq 124 ] && echo "TIMEOUT $name: ubox $* ran longer than $LIMIT s"
  if cmp -s got.out want.out && [ $got -eq $want ]; then pass=$((pass + 1))
  else fail=$((fail + 1)); echo "FAIL $name: $* (status $got, wanted $want)"; diff got.out want.out | head -5; cat got.err; fi
}
# check NAME EXPECTED COMMAND...: a shell command whose output must be EXPECTED
check() {
  name=$1; want=$2; shift 2
  selected "$name" || return 0
  start=$(date +%s%N)
  got=$(eval "timeout $LIMIT sh -c '$(printf '%s' "$*" | sed "s/'/'\\\\''/g")'" 2>&1)
  [ -n "$VERBOSE" ] && printf '%-16s %5d ms\n' "$name" $((($(date +%s%N) - start) / 1000000))
  if [ "$got" = "$want" ]; then pass=$((pass + 1)); else fail=$((fail + 1)); echo "FAIL $name: got '$got', wanted '$want'"; fi
}

case_ echo '' echo hello "big  world"
case_ echo-n '' echo -n no newline
case_ cat '' cat fruit.txt words.txt
case_ cat-stdin 'from stdin
' cat
case_ cat-binary '' cat random.bin
case_ tac '' tac fruit.txt
case_ rev '' rev words.txt
case_ nl-b 'a
b
c
' nl
case_ head '' head -n 2 fruit.txt
case_ head-default '' head fruit.txt
case_ tail '' tail -n2 fruit.txt
case_ wc '' wc fruit.txt words.txt
case_ wc-l '' wc -l fruit.txt
case_ wc-stdin 'one two
three
' wc -w
case_ grep '' grep apple fruit.txt
case_ grep-in '' grep -i -n FIG fruit.txt
case_ grep-vc '' grep -vc red fruit.txt
case_ grep-none '' grep kiwi fruit.txt
case_ sort '' sort fruit.txt
case_ sort-u '' sort -ru fruit.txt
case_ uniq '' uniq fruit.txt
case_ uniq-c '' uniq -c fruit.txt
case_ cut '' cut -d : -f 1,3 fruit.txt
case_ cut-range '' cut -d: -f2- fruit.txt
case_ tr 'Hello World
' tr a-z A-Z
case_ tr-d 'Hello World
' tr -d lo
case_ tr-nl 'a b c' tr ' ' '\n'
case_ seq '' seq 5
case_ seq-step '' seq 10 -3 1
case_ ls '' ls tree
case_ ls-aF '' ls -aF tree/sub
case_ ls-a-names '' ls tree/sub
case_ basename '' basename /usr/lib/x.so
case_ dirname '' dirname /usr/lib/x.so
case_ pwd '' pwd
case_ printenv '' printenv HOME
case_ printenv-unset '' printenv NO_SUCH_VARIABLE_HERE
case_ base64 '' base64 random.bin
case_ base64-w0 '' base64 -w 0 random.bin
case_ base64-stdin 'any carnal pleas' base64
case_ factor '' factor 360 97 1000000007 123456789012345678
case_ true '' true
case_ false '' false

check sort-n "2 3 10 10 25" "$UBOX sort -n -t: fruit.txt 2>/dev/null | head -0; cut -d: -f2 fruit.txt | $UBOX sort -n | $UBOX tr '\n' ' ' | sed 's/^ *//; s/ *\$//'"
check find "same" "$UBOX find tree > f1.txt; find tree -type f | sort > f2.txt; cmp -s f1.txt f2.txt && echo same"
check base64-roundtrip "same" "$UBOX base64 random.bin | $UBOX base64 -d | cmp -s - random.bin && echo same"
check base64-system "same" "base64 random.bin | $UBOX base64 -d | cmp -s - random.bin && echo same"
check tee "copy" "echo copy | $UBOX tee t1.txt t2.txt > /dev/null; cat t1.txt; cmp -s t1.txt t2.txt || echo differ"
check cp "same" "$UBOX cp random.bin copy.bin && cmp -s random.bin copy.bin && echo same"
check cp-into-dir "x" "$UBOX cp tree/a.txt tree/sub && cat tree/sub/a.txt"
check mv "gone moved" "$UBOX mv copy.bin moved.bin; [ -e copy.bin ] || printf 'gone '; [ -e moved.bin ] && echo moved"
check rm "0" "$UBOX rm moved.bin; echo \$?\$(ls moved.bin 2>/dev/null)"
check rm-missing "2" "$UBOX rm no-such-file 2>/dev/null; echo \$?"
check rm-f "0" "$UBOX rm -f no-such-file; echo \$?"
check rm-dir "2" "$UBOX rm tree/sub 2>/dev/null; echo \$?"
check rm-r "0 gone" "$UBOX rm -r tree/sub; printf '%s ' \$?; [ -e tree/sub ] || echo gone"
check date "UTC" "$UBOX date | cut -d' ' -f5-6 | sed 's/.* UTC/UTC/; s/^\\(UTC\\).*/\\1/'"
check unknown "2" "$UBOX frobnicate 2>/dev/null; echo \$?"
check bad-option "ubox: unknown option -z" "$UBOX sort -z fruit.txt"
check pipeline "      3 red" "$UBOX cut -d: -f3 fruit.txt | $UBOX grep -v '^\$' | $UBOX sort | $UBOX uniq -c | $UBOX sort -rn | $UBOX head -n 1"

echo "$pass passed, $fail failed"
[ $fail -eq 0 ]
