module Pat = Canonical.Patternctxt
module L = Canonical.Warnloc
module H = Canonical.Hint
             

let pass_checks (store: H.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pat.pc_of_expr f expr in
  let checks = Checkers.checks in
  List.iter (fun check -> check store pc) checks
  
