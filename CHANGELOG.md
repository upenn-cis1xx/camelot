# Changelog

## 1.7.0 (2021-03-01)

  Fix undesired `_ = None | None = _` style warnings

  * Only flags `_ = Some _ | Some _ = _` option style violations now. 
  * Also updates README to mention how Camelot is currently supported by 
    OCaml < 4.13.0 (for now).

## 1.6.2 (2021-02-08)

	Match Case Improvements (PR#81)

	* Improved student-facing "fix" for `x :: []` match patterns.
      Now includes the preferred syntax (`[x]`) that should be used 
      in place of the above.
    * Added new tests for the match patterns.
    * Add ocaml.4.11.1 to CI runners.
    * Added CHANGELOG.md (finally)
