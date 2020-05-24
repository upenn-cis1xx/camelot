(**
   HOF's passed to the iterator that run the linting rules.
*)
open Canonical



(** Secret dependency on arthur here - we pull up the config at this point, since
    arthur files only care about 
*)

let pass_exprs (store: Hint.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pctxt.ctxt_of_expr f expr in
  (* Fetch the lint config *)
  let cfg = Arthur.Basic.get_config () in
  let checks = Style.Checkers.expr_checks |> Arthur.Basic.arthur_disable cfg
             |> List.map snd in
  List.iter (fun check -> check store pc) checks

let pass_structures (store: Hint.hint list ref) (f: string) (structure : Parsetree.structure_item) : unit =
  let pc = Pctxt.ctxt_of_structure f structure in
  (* Fetch the lint config *)
  let cfg = Arthur.Basic.get_config () in
  let checks = Style.Checkers.struct_checks |> Arthur.Basic.arthur_disable cfg
             |> List.map snd in
  List.iter (fun check -> check store pc) checks

  
