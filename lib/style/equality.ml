open Canonical
open Utils
open Astutils
open Check

    
(* ------------------ Checks rules: if (_ = [literals] | [literals] = _)  ----------------------- *)
module EqList : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a pattern match to check whether a list has a certain value"
  let violation = "using `=` with lists as a condition in an if statement"
  let check st (E {location; source; pattern}: ctxt) = 
    begin match pattern with
      | Pexp_ifthenelse (cond, _, _) ->
        begin match cond.pexp_desc with
      | Pexp_apply (application, [(_,lop); (_,rop)]) ->
        if application =~ "=" && (is_list_lit lop || is_list_lit rop) then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
        end
      | _ -> ()
    end
  let name = "EqList", check
end


(* ------------------ Checks rules: _ = [Some _ | None] | [Some _ | None] = _ -------------  *)

module EqOption : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a pattern match to check the presence of an option"
  let violation = "using `=` with options"
  let check st (E {location; source; pattern}: ctxt) =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); (_, rop)]) ->
        if application =~ "=" && (is_option_lit lop || is_option_lit rop) then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
    let name = "EqOption", check
end
