open Canonical
open Utils
open Astutils
open Check

(* A pattern match that is considered long enough to override usual checks*)
let short_pattern_match = 2
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
        else if enable_unwrap then 
          List.concat_map (fun (c: Parsetree.case) -> unwrap_tuple c.pc_lhs) cases |>
          List.filter pred |>
          List.iter (fun (p: Parsetree.pattern) -> gen_error location source p st)
        else 
          List.map (fun (c: Parsetree.case) -> c.pc_lhs) cases |>
          List.filter pred |>
          (fun l -> try gen_error location source (List.hd l) st with _ -> ())
    | _ -> ()
    end
    
(** --------------------- Checks rules: match _ with | true | false ----------------------------- *)
module MatchBool : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using an if statement or boolean operators"
  let violation = "using pattern matching on boolean literals"
  let check = make_check (fun case -> is_case_constr case "true" || is_case_constr case "false") 
                         (fun location source _ st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
                         false

  let name = "MatchBool", check
end

(** --------------------- Checks rules: match _ with | 0 | n ------------------------------------ *)
module MatchInt : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using an if statement and `=`"
  let violation = "using integer pattern matching on fewer than " ^ (string_of_int long_pattern_match) ^ " cases"
  let check = make_check is_case_const 
                         (fun location source _ st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
                         false
  let name = "MatchInt", check
             
end

(** --------------------- Checks rules: match _ with | \{f1;f2;...\} ------------------------------ *)
module MatchRecord : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let statement to extract record fields"
  let violation = "using pattern matching on a record (for fewer than " ^ (string_of_int long_pattern_match) ^ " cases)"
  let check = make_check (fun pat -> is_pat_record pat)
                         (fun location source _ st -> st := Hint.mk_hint location source fix violation :: !st)
                         long_pattern_match
                         false
  let name = "MatchRecord", check
             
end 


(** --------------------- Checks rules: match _ with | (_, _ ..) -------------------------------- *)
module MatchTuple : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let statement to extract tuple fields"
  let violation = "using pattern matching on a tuple (for fewer than " ^ (string_of_int short_pattern_match) ^ " cases)"
  let check = make_check (fun pat -> is_pat_tuple pat)
                         (fun location source _ st -> st := Hint.mk_hint location source fix violation :: !st)
                         short_pattern_match
                         false
  let name = "MatchTuple", check
             
end


(** --------------------- Checks rules: match _ with | x :: [] ---------------------------------- *)
module MatchListVerbose : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "expressing this match case more compactly"
  let violation = "using an overly complex match clause"

  (* Predicate for checking that a match case looks like x :: [] *) 
  let pat_pred (pat: Parsetree.pattern) : bool =
    begin match pat.ppat_desc with
      | Ppat_construct ({txt = Lident "::";_}, Some matchcase) ->
        begin match matchcase.ppat_desc with
          | Ppat_tuple ([_; cons_case]) -> is_pat_constr cons_case "[]"
          | _ -> false
        end
      | _ -> false 
    end

  let check = make_check 
    (pat_pred)
    (fun location source pattern st -> 
      (* Regexp that matches literal(0 or more spaces)::(0 or more space)[] *)
      let matcher = Str.regexp "[a-zA-Z0-9_]+[ ]*::[ ]*\\[\\]" in
      let test s = try Str.search_forward matcher s 0 >= 0 with _ -> false in
      if pat_pred pattern then
        let refined_loc = Warn.warn_loc_of_loc location.file pattern.ppat_loc in
        let raw_source = IOUtils.code_at_loc refined_loc source in
        if test raw_source then
          st := Hint.mk_hint refined_loc ("| " ^ raw_source ^ " -> ...") fix violation :: !st 
    )
    Int.max_int
    true
   let name = "MatchListVerbose", check 
end
