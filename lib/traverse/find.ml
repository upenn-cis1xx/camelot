(**
   HOF's passed to the iterator that run the linting rules.
*)
open Canonical
open Parsetree

module Config = Arthur
let currently_linting : string ref = ref ""


(** Secret dependency on arthur here - we pull up the config at this point, since
    the linter needs to know what the arthur.json looks like! 
*)

let pass_exprs (store: Hint.hint list ref) (f: string) (expr : Parsetree.expression) : unit =
  let pc = Pctxt.ctxt_of_expr f expr in
  (* Fetch the lint config *)
  let config : Config.config = Config.get_config () in
  let checks = Config.eval_config
                      config
                      f
                      !currently_linting
                      Style.Checkers.expr_checks in
  List.iter (fun (_, check) -> check store pc) checks


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
  let config : Config.config = Config.get_config () in
  let checks = Config.eval_config
                      config
                      f
                      !currently_linting
                      Style.Checkers.struct_checks in
  List.iter (fun (_, check) -> check store pc) checks


let pass_file (store: Hint.hint list ref) (f: string) (_payload: Parsetree.structure) : unit =
  let pc = Pctxt.ctxt_for_lexical f (open_in f) in
  let config : Config.config = Config.get_config () in
  let checks = Config.eval_config
                      config
                      f
                      !currently_linting
                      Style.Checkers.lexical_checks in
  List.iter (fun (_, check) -> check store pc) checks
