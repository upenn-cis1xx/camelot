open Canonical
open Utils
open Astutils
    open Check
(* A pattern match that is considered long enough to override usual checks*)
let long_pattern_match = 5

let make_check (pred: Parsetree.case -> bool) gen_error override_len = 
  fun st (E {location; source; pattern} : Parsetree.expression_desc Pctxt.pctxt) -> 
    begin match pattern with
      | Pexp_match (_, cases) -> 
          if List.length cases >= override_len then () else
          if List.find_opt pred cases <> None then gen_error location source st
      | _ -> ()
    end

module MatchBool : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using an if statement"
  let violation = "using pattern matching when `=` is better"
  let check = make_check (fun case -> is_case_constr case "true" || is_case_constr case "false") 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
end

module MatchInt : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using an if statement and `=`"
  let violation = "using pattern matching when `=` is better"
  let check = make_check is_case_const 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
end

module MatchRecord : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let pattern match statement to extract record fields"
  let violation = "using pattern matching on a record"
  let check = make_check (fun case -> is_pat_record case.pc_lhs)
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
end 


module MatchTuple : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let pattern match statement to extract tuple fields"
  let violation = "using pattern matching on a tuple"
  let check = make_check (fun case -> is_pat_tuple case.pc_lhs)
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         2
end

module MatchListVerbose : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "expressing this match case more compactly"
  let violation = "using an overly complex match clause"
  let check st (E ctxt : Parsetree.expression_desc Pctxt.pctxt) =

    (* Predicate for checking that a match case looks like x :: [] *) 
    let case_pred (case: Parsetree.case) : bool =
      begin match case.pc_lhs.ppat_desc with
        | Ppat_construct ({txt = Lident "::";_}, Some matchcase) ->
          begin match matchcase.ppat_desc with
            | Ppat_tuple ([_; cons_case]) ->

              is_pat_constr cons_case "[]"
            | _ -> false
          end
        | _ -> false 
      end in
    (* Wrapper for List.find_opt  *)
    let contains_case pred cases = List.find_opt pred cases in

    (* Regexp that matches literal(0 or more spaces)::(0 or more space)[] *)
    let matcher = "[a-zA-Z_]+[ ]*::[ ]*\\[\\]" |> Str.regexp in

    (* Utility for checking for a given match  *)
    
    let test s = try Str.search_forward matcher s 0 >= 0 with _ -> false in
    begin match ctxt.pattern with
      | Pexp_match (_, cases) ->
        begin match contains_case case_pred cases with
        | None -> ()
        | Some c ->
          let refined_loc = Warn.warn_loc_of_loc ctxt.location.file c.pc_lhs.ppat_loc in
          let raw_source = IOUtils.code_at_loc refined_loc ctxt.source in
          if test raw_source then
            st := Hint.mk_hint refined_loc ("| " ^ raw_source ^ " -> ...") fix violation :: !st 
        end
      | _ -> ()
    end
    
end
