open Canonical
    
module MatchBool : Check.CHECK = struct
  let fix = "using an if statement"
  let violation = "using pattern matching with booleans"
  let check st ({location; source; pattern} : Pctxt.patternctxt) = 
    let pred (case: Parsetree.case) =
      begin match case.pc_lhs.ppat_desc with 
        | Ppat_construct ({txt = Lident "false"; loc = _}, _)
        | Ppat_construct ({txt = Lident "true"; loc = _}, _) -> true
        | _ -> false
      end in 
    begin match pattern with
      | Pexp_match (_, cases) -> 
          if List.find_opt pred cases <> None then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
end

module MatchInt : Check.CHECK = struct
  let fix = "using an if statement and `=`"
  let violation = "using pattern matching with ints"
  let check st ({location; source; pattern} : Pctxt.patternctxt) = 
    let pred (case: Parsetree.case) =
      begin match case.pc_lhs.ppat_desc with 
        | Ppat_constant _ -> true
        | _ -> false
      end in 
    begin match pattern with
      | Pexp_match (_, cases) -> 
          if List.find_opt pred cases <> None then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
end

