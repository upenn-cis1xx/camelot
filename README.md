# Camelot
An OCaml Linter / Style Checker for the OCaml compiler version 4.09.0.
Make sure you have ocaml version 4.09.0, otherwise the parsetree will be different

## Acknowledgements
This project wouldn't have been possible without the following three repos:

[sml-style-check](https://github.com/jluningp/sml-style-check) from the folks at CMU: for guiding the design of the linter, as well as basically contributing the structure of our project (especially the extendable checker modules :) ),

[hlint](https://github.com/ndmitchell/hlint) for a good reference on building a linter,

and [ppx_tools/rewriter](https://github.com/ocaml-ppx/ppx_tools/blob/master/rewriter.ml), for giving us useful starter code and being the building block on which camelot started. 

## Dependencies 
- dune >= 2.5.0
- ocaml-base-compiler >= v4.09.0
- compiler-libs.common
- fswatch (for Build + Watch)
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

`-show <ta | student | gradescope>` : Specify the reporting type - does a student see this output or a ta?
If this argument is malformed or not present, the reporting type defaults to student

`-f <filename>` : Lints the given file

