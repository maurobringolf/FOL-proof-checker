# FOL proof checker

![](https://github.com/maurobringolf/FOL-proof-checker/workflows/CI/badge.svg)

A proof checker for a proof calculus of first order logic.

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

## Contribution

I am open to contributions, feel free to open an issue or submit a pull request.
