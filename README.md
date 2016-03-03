# ulang

A small but not minimal functional language and theorem prover.

Current status: broken.

See branch [refactor-parser](https://github.com/gernst/ulang/tree/refactor-parser) for recent developments.

## Example Proof

Run `ulang/shell/Repl.scala` and enter:

    import prop
    :prove
    p → p or q
    simplify

