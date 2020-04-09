# Camelot
A fully-modular, ppx-based OCaml style linter.

## Dependencies 
Install the following with `opam install` after creating a switch:
- ocamlfind
- ppx_tools

### Note:
This project is dependent on compiler-libs, an inherently unstable library that
changes between OCaml installations.

### Overview of command line options
`make linter` to build the linter, and `make lint` to run the linter on the files in the
TOLINT Makefile variable.