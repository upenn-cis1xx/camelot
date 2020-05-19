open Canonical
open Astutils

let make_check pred gen_error = 
  fun st ({location; source; pattern} : Pctxt.patternctxt) -> 
    begin match pattern with
      | Pexp_match (_, cases) -> 
          if List.find_opt pred cases <> None then gen_error location source st
      | _ -> ()
    end

module MatchBool : Check.CHECK = struct
  let fix = "using an if statement"
  let violation = "using pattern matching when `=` is better"
  let check = make_check (fun case -> is_case_constr case "true" || is_case_constr case "false") 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
end

module MatchInt : Check.CHECK = struct
  let fix = "using an if statement and `=`"
  let violation = "using pattern matching when `=` is better"
  let check = make_check is_case_const 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
end


(* TODO: Find a better space for this code *)


let stream_of_chan file =
  let in_channel = open_in file in
  Stream.from
    (fun _ -> try Some (input_line in_channel) with End_of_file -> None)


let raw_at_line (line: int) s =
  let rec progress (read: int) s =
    if read = line - 1 then
      try Stream.next s with _ -> ""
    else
      (Stream.junk s; progress (read + 1) s)
  in
  progress 0 s

  
  
let raw_of_loc file line colstart colend =
  file |>
  stream_of_chan |>
  raw_at_line line |>
  (fun s -> String.sub s colstart (colend - colstart + 1))


let case_pred (case: Parsetree.case) : bool =
  begin match case.pc_lhs.ppat_desc with
    | Ppat_construct ({txt = Lident "::";_}, Some matchcase) ->
      begin match matchcase.ppat_desc with
        | Ppat_tuple ([_; cons_case]) -> is_pat_constr cons_case "[]"
        | _ -> false
      end
    | _ -> false 
  end

let matcher = "[a-z]+[ ]*::[ ]*\\[\\]" |> Str.regexp

let test s = try Str.search_forward matcher s 0 >= 0 with _ -> false

let contains_case pred cases = List.find_opt pred cases

module MatchListVerbose : Check.CHECK = struct
  let fix = "expressing this match case more compactly"
  let violation = "using an overly complex match clause"
  let check st (ctxt : Pctxt.patternctxt) =
    begin match ctxt.pattern with
      | Pexp_match (_, cases) ->
        begin match contains_case case_pred cases with
        | None -> ()
        | Some c ->
          let refined_loc = Warn.warn_loc_of_loc ctxt.location.file c.pc_lhs.ppat_loc in
          let raw_of_loc = raw_of_loc refined_loc.file refined_loc.line_start refined_loc.col_start refined_loc.col_end in
          if test raw_of_loc then
            st := Hint.mk_hint refined_loc ("| " ^ raw_of_loc ^ " -> ...") fix violation :: !st 
        end
      | _ -> ()
    end
    
end

