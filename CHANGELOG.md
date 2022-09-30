# Changelog

## 2.0.0 (2022-09-29)

  Move from supporting OCaml 4.10.0 to instead support OCaml 4.13.0:
  * Migrate from `Stream` in the stdlib to `camlp-streams`, an opam package with the same functionality.
  * `Parsetree` was updated and required code changes:
    * Support for `Pwith_modtype` and `Pwith_modtypesubst` variants.
    * Support for `Psig_modtypesubst` variant.
    * Rearranging some type deconstruction for `Ppat_construct` which was restructured slightly.

## 1.7.1 (2022-02-01)

  Fixes: 
  * Type constructor equality bugs such as `x = Some 5 || x = Some 6`.
    The above expression would show a warning to simplify the `||` statement
    because it didn't check that the values inside the `Some`s are the same.
  * Descent down trees of boolean expressions with `||` and `&&`s. If two
    expressions in the tree are the same, it should warn the user that it can be
    simplified.

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
