open Parsetree
open Astutils

module Pat = Canonical.Patternctxt
module L = Canonical.Warnloc


let find_exprs (pats: Pat.patternctxt list ref) (f: string) (expr: Parsetree.expression) : unit =
  let location = L.warn_loc_of_loc f expr.pexp_loc in
  let source = Pprintast.string_of_expression expr in
  match expr.pexp_desc with
  | Pexp_apply (e, [_; _]) ->
    if e =~ "=" then
      pats := {location; source; pattern=expr.pexp_desc} :: !pats;
  | _ -> ()

      
