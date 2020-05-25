(**
   HOF's passed to the iterator that run the linting rules.
*)
open Canonical


let cfg = Arthur.Basic.get_config ()


let expr_checks =
  Style.Checkers.expr_checks |>
  Arthur.Basic.arthur_disable cfg

let struct_checks =
  Style.Checkers.struct_checks |>
  Arthur.Basic.arthur_disable cfg


(** Secret dependency on arthur here - we pull up the config at this point, since
    the linter needs to know what the arthur.json looks like! 
*)

let pass_exprs (store: Hint.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pctxt.ctxt_of_expr f expr in
  (* Fetch the lint config *)
  List.iter (fun check -> check store pc) expr_checks

let pass_structures (store: Hint.hint list ref) (f: string) (structure : Parsetree.structure_item) : unit =
  let pc = Pctxt.ctxt_of_structure f structure in
  List.iter (fun check -> check store pc) struct_checks

  
