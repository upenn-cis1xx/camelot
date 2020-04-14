# Camelot
An extensible OCaml Linter / Style Checker

## Dependencies 
Install the following with `opam install` after creating a switch:
- ocamlfind (for debugging / dev)
- ocamlbuild
- ppx_tools
- compiler-libs
- ANSITerminal

### Note:
This project is dependent on compiler-libs, an inherently unstable library that
changes between OCaml installations.

### Overview of command line options
`make linter` to build `camelot`. This produces the `camelot` binary that can be run
through the command line.

For dev purposes the `make lint` rule is setup so that you can run the binary on files in a particular directory, as specified in the `TO_LINT` variable.