# Camelot ![Build and Test Status](https://github.com/upenn-cis1xx/camelot/workflows/Build%20&%20Test/badge.svg)
An OCaml Linter / Style Checker for the OCaml compiler version 4.10.0.
Make sure you have ocaml version 4.10.0, otherwise the parsetree will be different

## Acknowledgements
This project wouldn't have been possible without the following three repos:

[sml-style-check](https://github.com/jluningp/sml-style-check) from the folks at CMU: for guiding the design of the linter, as well as basically contributing the structure of our project (especially the extendable checker modules :) ),

[hlint](https://github.com/ndmitchell/hlint) for a good reference on building a linter,

and [ppx_tools/rewriter](https://github.com/ocaml-ppx/ppx_tools/blob/master/rewriter.ml), for giving us useful starter code and being the building block on which camelot started. 

## Dependencies
- ocaml >= v4.10.0
- dune >= 2.5.0
- compiler-libs.common
- fswatch (for Build + Watch)
- ANSITerminal
- ppx_expect >= 0.13.1 (for testing)
- yojson >= 1.7.0
- odoc (for documentation builds)

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

### Writing and running tests
To run tests:
`dune test`

If any changes you make break code, `dune test` will flag it and highlight the differences. If there are no issues,
dune will not print anything.

To write tests, see the dune documentation for [expect-tests](https://dune.readthedocs.io/en/stable/tests/html).
If you implement a new rule, you'll have to do the following:
* Add your test case ( a program that you expect the linter to show a match for) to either a new file or an existing file
  in the `examples/` directory.
  * If you create a new file in the `examples/` directory, edit the `dune` file's deps stanza to make it visible to the
  build system. If you create a new programs file in `examples/`, you'll have to add a new `let%expect_test` in the style of the ones prior to the
  file `test.ml`, except with the ``[%expect {| ... |} ]``` clause blank.
  * If you added a test program to an existing file, it'll automatically be linted by the appropriate expect_test.
* Run `dune runtest`. If your code for linting worked, `runtest` should
  highlight that there was a difference due to your new rule - run `dune promote` to accept this difference.
  Only promote if the difference that `dune` shows is appropriate - if there is a difference in the linted output, verify that your code did not break things,
  fix the issue, and then promote.
* Reloading test.ml should show that the difference was added to the appropriate expect tests. In a PR, mention this - the reviewer
  should examine the expect test and make sure it makes sense (e.g. that the old prints were not messed up + the new test case appropriately printed).

## Camelot flags

`-d <lintdir>` : Specify the directory in which to lint

`-show <ta | student | gradescope>` : Specify the reporting type - does a student see this output or a ta?
If this argument is malformed or not present, the reporting type defaults to student

`-f <filename>` : Lints the given file




