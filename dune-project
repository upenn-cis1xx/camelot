(lang dune 2.5)
(name camelot)
(generate_opam_files true)

(version 2.1.0)

(source (github upenn-cis1xx/camelot))
(license "Apache-2.0")
(authors "Vighnesh Vijay" "Daniel Like" "William Goeller")
(maintainers "William Goeller <william@williamgoeller.com>")

(package
 (name camelot)
 (synopsis "An OCaml Linter / Style Checker")
 (depends
  (dune (>= 2.5))
  (ocaml (and (>= 5.4.1) (< 5.5.0)))
  (ANSITerminal (>= 0.8))
  (yojson (>= 1.7.0))
  (camlp-streams (>= 5.0.1))
  (ppx_expect (and :with-test (>= v0.17.3)))
  (odoc (and :with-doc (>= 1.5.0)))
 )
)
