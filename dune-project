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
 (synopsis "Core types for the camelot linter internals")
)

(package
 (name camelot-report)
 (version 0.2)
 (synopsis "Output modules for reporting and grading linted code")
)

(package
 (name camelot-style)
 (version 0.2)
 (synopsis "Style Checking Modules implemented here")
)

(package
 (name camelot-traverse)
 (version 0.2)
 (synopsis "Core modules for traversing the OCaml AST and calculating the style hints")
)

(package
 (name camelot-utils)
 (version 0.2)
 (synopsis "Utility modules for extracting information from the AST and the raw ml file for that AST ")
)