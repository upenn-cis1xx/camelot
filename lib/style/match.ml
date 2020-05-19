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
  let check = make_check (fun case -> is_pat_constr case "true" || is_pat_constr case "false") 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
end

module MatchInt : Check.CHECK = struct
  let fix = "using an if statement and `=`"
  let violation = "using pattern matching when `=` is better"
  let check = make_check is_pat_const 
                         (fun location source st -> st := Hint.mk_hint location source fix violation :: !st)
end

