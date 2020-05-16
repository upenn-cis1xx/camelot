# Camelot
An OCaml Linter / Style Checker for the OCaml compiler version 4.09.0.
Make sure you have ocaml version 4.09.0, otherwise the parsetree will be different

## Dependencies 
- dune (v 2.4.0) - dune will manage installing the appropriate dependencies for you I believe
- ocamlc v4.09.0
- ocamlfind (for debugging / dev)
- compiler-libs.common
- ANSITerminal

### Note:
This project is dependent on compiler-libs, an inherently unstable library that
changes between OCaml installations.

### Build and run instructions
Build:
`dune build bin/camelot.exe`

Build + Watch:
`dune build bin/camelot.exe -w`

Run 'tests':
`dune exec -- bin/camelot.exe <camelot args here>`

## Camelot flags

`-d <lintdir>` : Specify the directory in which to lint

`-show <ta | student>` : Specify the reporting type - does a student see this output or a ta?
If this argument is malformed or not present, the reporting type defaults to student