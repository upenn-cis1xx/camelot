open Warnloc

type patternctxt = {
  location: warn_loc;
  source: string;
  pattern: Parsetree.expression_desc
}


let mk_pc location source pattern = {location; source; pattern}

let pc_of_expr (f: string) (expr: Parsetree.expression) : patternctxt = 
  let location = warn_loc_of_loc f expr.pexp_loc in
  let source = Pprintast.string_of_expression expr in
  {location; source; pattern = expr.pexp_desc}
