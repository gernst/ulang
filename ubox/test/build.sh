#!/bin/sh
# builds out/ubox/ubox.bin and out/ubox/ubox.sh, run from the root of the repository after make; CC is the compiler,
# which needs the headers of GMP and the collector
set -e
mkdir -p out/ubox
out/ulang.sh compile ubox/ubox.u -o out/ubox/ubox.c
${CC:-cc} -O2 -DULANG_LIB="\"$PWD/ulang/prelude\"" -I ulang/runtime out/ubox/ubox.c ulang/runtime/ulang.c -lgmp -lgc -o out/ubox/ubox.bin
cp bootstrap/ulang.sh out/ubox/ubox.sh
