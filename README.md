# FOL proof checker

![](https://github.com/maurobringolf/FOL-proof-checker/workflows/CI/badge.svg)

A proof checker for a proof calculus of first order logic.

This program reads formal proofs in the language of **first order logic (FOL)** and checks them for correctness.
I wrote this for fun and educational purposes only, essentially to internalise two things:

1. Formal proofs can be checked mechanically.
1. A conventional math proof is a long shot from a formal proof.

## Syntax

A proof text contains three optional sections:

1. Signature
2. Theory
3. Proof

Here is an example which uses all of them:

    #relations: φ(0)
    φ -> φ
    |-
    φ -> φ
    (φ -> φ) -> ((φ -> φ) -> (φ <-> φ))
    (φ -> φ) -> (φ <-> φ)
    φ <-> φ

As its signature it defines a 0-ary relation `φ` (effectively a propositional variable; it can be assigned true or false).
All formulae before the `|-`-symbol go into the proof context, i.e. these are taken as the non-logical axioms.
Finally, the sequence of formulae after `|-` is a proof in the sense of the next section.

### Signature

The signature can be specified as a preamble to the proof as follows:

    #constants: 0, 1, a, b, c
    #functions: f(1), g(4)
    #relations: R(0), S(2), T(1)

Where numbers in parenthesis are the arities.
Everything is untyped and there is only one domain for values.

## Proof calculus

The axioms and inference rules can be found under [docs/proof-calculus.pdf](docs/proof-calculus.pdf).
A proof is just a sequence of formulae, each of which either:

* follows from two previous ones by modus ponens
* follows from a previous one by generalisation
* is an instance of a logical axiom
* is a non-logical axiom

There is also some (so far only very basic) syntax to define the signature and theory a proof uses.
For convenience, **Peano arithmetic (PA)** is predefined and can be "imported" using the preamble `#PA` (See for example [test/proofs/correct/example-1.2.proof](test/proofs/correct/example-1.2.proof)).

## CLI interface

The program takes as input a path to a text file containing a proof and checks it.
If the proof is not correct, it stops at the *first* incorrect step.
I compile and run this with [stack](https://docs.haskellstack.org/en/stable/README/):

```
$> stack build
$> stack exec FOL-proof-checker-exe test/proof/correct/simple-modus-ponens.proof
Correct
$>
```

