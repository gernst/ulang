#!/bin/sh
# runs the binary of the same name next to this script, ulang.sh the ulang.bin, with the large stack that the interpreter and
# the compiler need for deep recursion; make copies it next to each binary that it builds and runs
ulimit -s unlimited 2>/dev/null || ulimit -s hard
exec "${0%.sh}.bin" "$@"
