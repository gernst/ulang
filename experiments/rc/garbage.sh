#!/bin/bash
# the cells that nothing reaches at the exit of a counted C program but that the counts never freed, sampled, by
# the line of the C that allocated them: garbage.sh <program.c> <args...>
S=${RC_OUT:-$(git rev-parse --show-toplevel)/out/rc}; mkdir -p $S/bis && export S
cd $(git rev-parse --show-toplevel)
python3 - <<'EOF'
s=open('ulang/runtime/ulang.c').read()
track='''
static long long allocations, garbage;
static void unreachable(void *cell, void *site) {
  garbage++;
  fprintf(stderr, "garbage %p\\n", (void *)((char *)site - (char *)u_mk_con_2));
}
static void track(void *cell, void *site) {
  if (allocations++ % RATE) return;
  GC_register_finalizer_no_order(cell, (GC_finalization_proc)unreachable, site, NULL, NULL);
}
static void untrack(void *cell) {
  GC_register_finalizer_no_order(cell, NULL, NULL, NULL, NULL);
}
__attribute__((destructor)) static void collect_garbage(void) {
  GC_enable();
  GC_gcollect();
  GC_invoke_finalizers();
  fprintf(stderr, "sampled garbage %lld of %lld sampled allocations\\n", garbage, allocations / RATE);
}
'''
def rep(old,new):
    global s
    assert s.count(old)==1,old; s=s.replace(old,new)
rep('// an object with its items after it, in one allocation', track + '\n// an object with its items after it, in one allocation')
rep('static u_value compound(const struct u_type *type, int size) {','__attribute__((noinline)) static u_value compound(const struct u_type *type, int size) {')
rep('  U_RC_STAT(9);','  U_RC_STAT(9);\n  track(v, __builtin_return_address(1));')
rep('static void recycle(u_value v) {','static void recycle(u_value v) {\n  untrack(v);')
rep('  GC_INIT();','  GC_INIT();\n  GC_disable();')
open(__import__('os').environ['S'] + '/bis/ulang_garbage.c', 'w').write(s)
EOF
cp ulang/runtime/ulang.h ulang/runtime/window.c $S/bis/
src=$1; shift
nice -n 19 clang -O${OPT:-1} -g -fno-omit-frame-pointer -fno-optimize-sibling-calls -std=c11 -DRATE=${RATE:-64} -Wno-unused-variable -Wno-unused-function -I $S/bis -DULANG_LIB='"'$PWD/ulang/prelude'"' $src $S/bis/ulang_garbage.c -lgmp -lgc -o $S/bis/garbage.bin 2>&1 | grep error
(ulimit -s unlimited; ulimit -v 16000000; timeout 600 $S/bis/garbage.bin "$@" > /dev/null 2> $S/bis/garbage.txt)
grep "^sampled garbage" $S/bis/garbage.txt
base=$(nm $S/bis/garbage.bin | awk '$3=="u_mk_con_2"{print $1}')
grep '^garbage' $S/bis/garbage.txt | awk '{print $2}' | sort | uniq -c | sort -rn | head -40 | while read n off; do a=$(printf "%x" $((0x$base + off))); echo "$n $a $(addr2line -f -e $S/bis/garbage.bin $a | paste -sd' ' | sed 's|/[^ ]*/||')"; done
