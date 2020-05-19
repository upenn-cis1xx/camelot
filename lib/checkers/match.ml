module A = Astutils
module H = Canonical.Hint
module C = Canonical.Patternctxt
open Parsetree
open Check
open C
open List

module MatchBool : Check.CHECK = struct
  let fix = "using an if statement"
  let violation = "using pattern match with booleans"
  let check st {location; source; pattern} = 
    let pred (case: Parsetree.case) =
      begin match case.pc_lhs.ppat_desc with 
        | Ppat_constant _
        | Ppat_construct ({txt = Lident "false"; loc = _}, _)
        | Ppat_construct ({txt = Lident "true"; loc = _}, _) -> true
        | _ -> false
      end in 
    begin match pattern with
      | Pexp_match (_, cases) -> 
          if List.find_opt pred cases <> None then
          st := H.mk_hint location source fix violation :: !st
      | _ -> ()
    end
end