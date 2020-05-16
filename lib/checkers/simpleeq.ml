module A = Astutils
module H = Canonical.Hint
module C = Canonical.Patternctxt

open Check
open C
open A

(* ------------------ Checks rules: _ = [literals] | [literals] = _  ----------------------- *)
module EqList : Check.CHECK = struct
  let fix = "using a pattern match to check whether a list has a certain value"
  let violation = "using `=` with lists"
  let check st {location; source; pattern} = 
    begin match pattern with
      | Pexp_apply (application, [(_,lop); (_,rop)]) ->
        if application =~ "=" && (A.is_list_lit lop || A.is_list_lit rop) then
          st := H.mk_hint location source fix violation :: !st
      | _ -> ()
    end
end


(* ------------------ Checks rules: _ = [Some _ | None] | [Some _ | None] = _ -------------  *)

module EqOption : Check.CHECK = struct
  let fix = "using a pattern match to check the presence of an option"
  let violation = "using `=` with options"
  let check st {location; source; pattern} =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); (_, rop)]) ->
        if application =~ "=" && (A.is_option_lit lop || A.is_option_lit rop) then
          st := H.mk_hint location source fix violation :: !st
      | _ -> ()
    end
end
