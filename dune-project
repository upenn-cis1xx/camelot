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
  (ocamlc (>= 4.09.0))
  (ocamlfind (>= 1.8.1))
  (camelot-canonical (>= 0.2))
  (camelot-report (>= 0.2))
  (camelot-style (>= 0.2))
  (camelot-traverse (>= 0.2))
  (camelot-utils (>= 0.2))
  )
)

(package
 (name camelot-canonical)
 (version 0.2)
 (synopsis "Part of the Camelot package")
)

(package
 (name camelot-report)
 (version 0.2)
 (synopsis "Part of the Camelot package")
)

(package
 (name camelot-style)
 (version 0.2)
 (synopsis "Part of the Camelot package")
)

(package
 (name camelot-traverse)
 (version 0.2)
 (synopsis "Part of the Camelot package")
)

(package
 (name camelot-utils)
 (version 0.2)
 (synopsis "Part of the Camelot package")
)