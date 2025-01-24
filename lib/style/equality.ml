open Canonical
open Utils
open Astutils
open Check


(** ------------------ Checks rules: if (_ = [literals] | [literals] = _)  ----------------------- *)
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


(** ------------------ Checks rules: _ = [None] | [None] = _ -------------  *)
module EqOption : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a pattern match to check the presence of an option"
  let violation = "using `=` with options"
  let check st (E {location; source; pattern}: ctxt) =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); (_, rop)]) ->
        if application =~ "=" && (is_some_lit lop || is_some_lit rop) then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "EqOption", check
end

(** ------------------ Checks rules: (_ = :bool | :bool = _)  ----------------------- *)
module EqBool : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using the variable itself to represent the value"
  let violation = "using `=` with a boolean literal"
  let check st (E {location; source; pattern}: ctxt) = 
    begin match pattern with
      | Pexp_apply (application, [(_,lop); (_,rop)]) ->
        if application =~ "=" && (is_bool_lit lop || is_bool_lit rop) then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "EqBool", check
end

(** ------------------ Checks rules: (_ == _)  ----------------------- *)
module EqPhysical : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using `=` to evaluate structural equality"
  let violation = "using `==` when structural equality is intended"
  let check st (E {location; source; pattern}: ctxt) =
    begin match pattern with
      | Pexp_apply (application, [(_,_); (_,_)]) ->
        if application =~ "==" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "EqPhysical", check
end