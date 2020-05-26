(**
   HOF's passed to the iterator that run the linting rules.
*)
open Canonical
open Parsetree

(* force computation *)
let cfg = Arthur.parse ()


let currently_linting : string ref = ref ""


(** Secret dependency on arthur here - we pull up the config at this point, since
    the linter needs to know what the arthur.json looks like! 
*)

let pass_exprs (store: Hint.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pctxt.ctxt_of_expr f expr in
  (* Fetch the lint config *)
  let expr_checks =
  Style.Checkers.expr_checks |> Arthur.extract ( Lazy.force cfg ) in
  
  let checks = expr_checks |> Arthur.refine (Lazy.force cfg) !currently_linting in
  List.iter (fun check -> check store pc) checks


let set_toplevel : Parsetree.structure_item -> unit = fun i ->
 begin match i.pstr_desc with
   | Pstr_value (_, [vb]) ->
     begin match vb.pvb_pat.ppat_desc with
       | Ppat_var {txt = i; loc = _} -> currently_linting := i
       | _ -> ()
     end
   | _ -> ()
 end
 
    

let pass_structures (store: Hint.hint list ref) (f: string) (structure : Parsetree.structure_item) : unit =
  (* Flag the currently linted toplevel function *)
  set_toplevel structure;
  let pc = Pctxt.ctxt_of_structure f structure in

  let struct_checks =
  Style.Checkers.struct_checks |> Arthur.extract (Lazy.force cfg) in
  let checks = struct_checks |> Arthur.refine (Lazy.force cfg) !currently_linting in
  List.iter (fun check -> check store pc) checks

  
