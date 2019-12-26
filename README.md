# FOL proof checker

![](https://github.com/maurobringolf/FOL-proof-checker/workflows/CI/badge.svg)

A proof checker for a proof calculus of first order logic.

This program reads formal proofs in the language of **first order logic (FOL)** and checks them for correctness.
I developed this for fun and educational purposes only, but am nevertheless open to contributions or advice.

## Proof calculus

The axioms and inference rules can be found under [docs/proof-calculus.pdf](docs/proof-calculus.pdf).

## CLI interface

The CLI program takes as input a path to a text file containing a proof,
checks it and prints the results.
If the proof is not correct, the checker stops at *the first incorrect step*.
Using [stack](https://docs.haskellstack.org/en/stable/README/),
you can compile and run this project as follows:

```
stack build
stack exec FOL-proof-checker-exe path/to/some-proof.txt
```

