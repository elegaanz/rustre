# Rustre : Lustre v6 parser in Rust

## Crates

- rustre-parser: the main parser, emits an untyped AST
- rustre-ast-dump: reads a rustre file and dumps it as an untyped AST

## Compatibility with the official implementation

[Reference implementation](https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6)

All the tests of the reference implementation are successfully parsed by the Rust version.

You can check it by cloning the reference implementation next to this project and by running `./compat.sh`.

## Licence

GPLv3.0
