(lang dune 2.5)
(name camelot)
(generate_opam_files true)

(version 0.2)

(source (github upenn-cis1xx/camelot))
(license "Apache License 2.0")
(authors "Vighnesh Vijay" "Daniel Like" "William Goeller")
(maintainers "cis120@seas.upenn.edu")

(package
 (name camelot)
 (synopsis "An OCaml Linter / Style Checker")
 (depends
  (dune (>= 2.5))
  (ocaml-base-compiler (>= 4.09.0))
  (ocamlfind (>= 1.8.1))
 )
)