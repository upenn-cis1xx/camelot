(lang dune 2.5)
(name camelot)
(generate_opam_files true)

(version 1.7.0)

(source (github upenn-cis1xx/camelot))
(license "Apache-2.0")
(authors "Vighnesh Vijay" "Daniel Like" "William Goeller")
(maintainers "William Goeller <william@williamgoeller.com>")

(package
 (name camelot)
 (synopsis "An OCaml Linter / Style Checker")
 (depends
  (dune (>= 2.5))
  (ocaml (and (>= 4.10.0) (< 4.13.0)))
  (ANSITerminal (>= 0.8))
  (yojson (>= 1.7.0))
  (ppx_expect (and :with-test (>= v0.13.0)))
  (odoc (and :with-doc (>= 1.5.0)))
 )
)
