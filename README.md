# ulang

A small but not minimal functional language and theorem prover.

Current status: not entirely broken.

## Example Proof

Run `ulang/shell/Repl.scala` and enter:

    import prop
    :prove
    p → p or q
    simplify

## Syntax

Expression language

    expr e ::= x | op | λ x. e | e e

Type language

    type t ::= a | C t1 ... tn

with a predefined binary type constructor `→` for function types (written infix).
Functions with several arguments are curried and `→` associates to the right as usual,
whereas application associates to the left.

Syntax is mostly whitespace agnostic.
Each line represents a declaration, e.g.

    id x = x

However, long lines can be continued by indenting the next line, e.g.

    id x
      = x

Block comments are delimited by `/*` and `*/` and can be nested,
line comments start with `//` and extend to the next newline as usual.

There are several types of declarations. Their order in the file is mostly
irrelevant, the only exception being the order of constant declarations that
must match dependencies (in the future this may change).

Imports are evaluated first and make the whole signature of a submodule
available (transitively)

    import module

Mixfix declarations are evaluated can specify an identifier as prefix, postfix,
or infix (left, right or non associative), with a given precedence.
Normal applications bind tighter than mixfix syntax.
The same set of mixfix operators is used for types and expressions.

    prefix  prec id
    postfix prec id
    infix left  prec id
    infix right prec id
    infix       prec id

Types are either introduced as abbreviations from existing types or represent
inductive data types. By convention, type names are upper case.
Data types may be mutually recursive.

    type C a1 ... an
      = t

    data C a1 ... an
      = c1: t1 | ... | cn: tn

The type of functions/constants must be explicitly declared.
Arbitrary overloading is possible, however, for all expressions/definitions must
have exactly one type.

    op: t

Functions are defined by equations and pattern matching.
Several equations for the same function (as determined by overloading resolution
during type checking) are automatically grouped. The pattern match may discern
data type constructors (and possibly view patterns in the future).
Pattern matches must be deterministic.
Functions may be mutually recursive and it is possible to state higher order
equations without specifying all of the arguments.

    f (c ...) (d ...)
      = e
    
    h = g

There are very little restrictions in what names may be used for functions and
types, unicode characters are supported. As a consequence, identifiers must be
separated by spaces, i.e., `1+2` is parsed as a single identifier.
An exception holds for the punctuation characters `.`, `(`, `)` and lambda `λ`;
although `()` is parsed as an identifier not an empty pair of parentheses.

Reserved keywords and tokens

    import data type
    prefix postfix infix binder
    ( ) λ . ;

Additionally, the following tokens are not allowed for type names
(as they are used in the syntax of data type declarations)

    | =

## Semantics

Standard HOL is intended. To be made precise.

## Implementation

## Prelude

The prelude file defines basic data types and functions.

- Booleans of type `Bool` with logical operators (using unicode syntax).
- The unit type `()` with a single value `()`
- Option type `Option a` with constructors `some` and `none`
- List type `List a` with constructors `[]` and `::` (infix right)

## Discussion & Design Goals

The main goals are

- familiarity for programmers used to functional programming
- make it possible to express mathematical definitions and theorems the way one
  would present them in a paper or textbook
- simultaneously strive for simple syntactic rules, don't put in too much notation.
- minimizing clutter: no semicolons for example, no explicit match statements imposed
- have a feasible effort for an implementation

