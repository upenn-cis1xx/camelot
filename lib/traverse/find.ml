open Canonical             

let pass_checks (store: Hint.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pctxt.pc_of_expr f expr in
  let checks = Style.Checkers.checks in
  List.iter (fun check -> check store pc) checks
  
