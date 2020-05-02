open Parsetree
open Astutils
open Pprintast
open Style
open Report

(** Modify this file, checkers/report.ml,
    checkers/custom.ml 
    to extend the linter with custom rules
*)

(* These are refs used to store important information in the context *)
let pats : Style.patternctxt list ref = ref []
let hints : Style.hint list ref = ref []
let rules = Simpleeq.checks @ Simplebexp.checks
let file : string ref = ref ""




(** This is the handler function passed into linterator 
    It extracts patterns of interest so that we can match
    them against the predefined rules in checkers
*)

let patterns_of_interest (f: string) (expr: Parsetree.expression) : unit = 
  let {pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes} = expr in
  let location = Style.warn_loc_of_loc f pexp_loc in
  let source = Pprintast.string_of_expression expr in
  match pexp_desc with
  | Pexp_apply (e, [(_,el); (_,er)]) ->
    if is_id e "=" then 
      pats := {location; source; pattern = Style.EqApply (el.pexp_desc, er.pexp_desc)} :: !pats;
  | Pexp_ifthenelse (test,  next, Some default) -> 
    pats := 
      {location; source; pattern = Style.IfUse (test.pexp_desc, next.pexp_desc, default.pexp_desc) } :: !pats
  | _ -> ()


let lint : unit -> unit = fun _ ->
  List.iter (fun pctxt -> 
      List.iter (fun f -> f hints pctxt) rules
    ) !pats

let hint : unit -> unit = fun _ -> 
  List.iter (Report.print_hint) !hints
