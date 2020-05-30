open Canonical
open Utils
open Astutils
open Check

(* A pattern match that is considered long enough to override usual checks*)
let long_pattern_match = 3

let make_check (pred: Parsetree.pattern -> bool) gen_error override_len enable_unwrap = 
  fun st (E {location; source; pattern} : Parsetree.expression_desc Pctxt.pctxt) -> 

    let rec unwrap_tuple (p : Parsetree.pattern) : Parsetree.pattern list =
      begin match p.ppat_desc with
      | Ppat_tuple pat_list -> List.concat_map unwrap_tuple pat_list
      | _ -> [p]
      end 
    in

    begin match pattern with
    | Pexp_match (_, cases) -> 
        if List.length cases >= override_len then () 
        else if None <> List.find_opt pred @@
                          if enable_unwrap 
                          then List.concat_map (fun (c: Parsetree.case) -> unwrap_tuple c.pc_lhs) cases
                          else List.map (fun (c: Parsetree.case) -> c.pc_lhs) cases
        then gen_error location source st
    | _ -> ()
    end
    

module MatchBool : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using an if statement"
  let violation = "using pattern matching when `=` is better"
  let check = make_check (fun case -> is_case_constr case "true" || is_case_constr case "false") 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
                         true

  let name = "MatchBool", check
end

module MatchInt : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using an if statement and `=`"
  let violation = "using pattern matching when `=` is better"
  let check = make_check is_case_const 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
                         true
  let name = "MatchInt", check
             
end

module MatchRecord : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let pattern match statement to extract record fields"
  let violation = "using pattern matching on a record"
  let check = make_check (fun case -> is_pat_record case)
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
                         true
  let name = "MatchRecord", check
             
end 


module MatchTuple : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let pattern match statement to extract tuple fields"
  let violation = "using pattern matching on a tuple"
  let check = make_check (fun case -> is_pat_tuple case)
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
                         2
                         false
  let name = "MatchTuple", check
             
end


module MatchListVerbose : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "expressing this match case more compactly"
  let violation = "using an overly complex match clause"
  let check st (E ctxt : Parsetree.expression_desc Pctxt.pctxt) =

    (* Predicate for checking that a match case looks like x :: [] *) 
    let pat_pred (pat: Parsetree.pattern) : bool =
      begin match pat.ppat_desc with
        | Ppat_construct ({txt = Lident "::";_}, Some matchcase) ->
          begin match matchcase.ppat_desc with
            | Ppat_tuple ([_; cons_case]) -> is_pat_constr cons_case "[]"
            | _ -> false
          end
        | _ -> false 
      end in
    (* Wrapper for List.find_opt  *)
    let contains_pattern pred patterns = List.find_opt pred patterns in

    (* Regexp that matches literal(0 or more spaces)::(0 or more space)[] *)
    let matcher = "[a-zA-Z_]+[ ]*::[ ]*\\[\\]" |> Str.regexp in

    (* Utility for checking for a given match  *)
    
    let test s = try Str.search_forward matcher s 0 >= 0 with _ -> false in
    begin match ctxt.pattern with
      | Pexp_match (_, cases) ->
        let patterns = List.map (fun (c: Parsetree.case) -> c.pc_lhs) cases in
        begin match contains_pattern pat_pred patterns with
        | None -> ()
        | Some p ->
          let refined_loc = Warn.warn_loc_of_loc ctxt.location.file p.ppat_loc in
          let raw_source = IOUtils.code_at_loc refined_loc ctxt.source in
          if test raw_source then
            st := Hint.mk_hint refined_loc ("| " ^ raw_source ^ " -> ...") fix violation :: !st 
        end
      | _ -> ()
    end
   let name = "MatchListVerbose", check 
end
