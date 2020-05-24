open Canonical             

let pass_exprs (store: Hint.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pctxt.ctxt_of_expr f expr in
  let checks = Style.Checkers.expr_checks in
  List.iter (fun check -> check store pc) checks

let pass_structures (store: Hint.hint list ref) (f: string) (structure : Parsetree.structure_item) : unit =
  let pc = Pctxt.ctxt_of_structure f structure in
  let checks = Style.Checkers.struct_checks in
  List.iter (fun check -> check store pc) checks

  
