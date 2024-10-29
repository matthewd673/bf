# bf

Bf is an OCaml implementation of the popular esoteric programming language
[brainfuck](https://esolangs.org/wiki/Brainfuck). The most interesting parts of
the implementation are probably the Parser and Vm modules. It uses run-length
encodings for instructions like `+` and `<` and parses `[...]` pairs into a
special `Loop` construct.

## Dependencies

Bf depends on:
- [dune](https://opam.ocaml.org/packages/dune/)
- [ounit2](https://opam.ocaml.org/packages/ounit2/)

## Build and run

To build, run, and test:

```
dune build
dune exec bf -- <filename>
dune runtest
```
