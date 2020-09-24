open Canonical
open Utils
open Astutils
open Check

(** ----------------------- Checks rules: [_] \@ _ ---------------------------- *)
module LitPrepend : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using `::` instead"
  let violation = "using `@` to prepend an element to a list"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); _]) ->
        if application =~ "@" && is_singleton_list lop then
          let raw = IOUtils.code_at_loc location source in
          st := Hint.mk_hint location raw fix violation :: !st
      | _ -> ()
    end
  let name = "LitPrepend", check
end


(** ----------------------- Checks rules: fst/snd t -------------------------- *)
module TupleProj : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using a let pattern match statement instead"
  let violation = "using fst / snd to project values out of a tuple"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (application, [_]) ->
        if application =~ "fst" || application =~ "snd" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "TupleProj", check
end

(** ----------------------- Checks rules: Nesting if >= 3 levels ------------- *)
module NestedIf : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using let statements or helper methods / rethinking logic"
  let violation = "using nested if statements more than three layers deep"
  let check st (E {location; source; pattern} : ctxt) =
    let rec find_nesting (p: Parsetree.expression_desc) depth= 
      depth = 0 ||
      begin match p with 
        | Pexp_ifthenelse (_, bthen, Some belse) -> 
          if depth = 1 then true else 
            find_nesting ((skip_seq_let bthen).pexp_desc) (depth - 1) ||
            find_nesting ((skip_seq_let belse).pexp_desc) (depth - 1) 
        | _ -> false
      end 
    in 
    if find_nesting pattern 4 then st := Hint.mk_hint location source fix violation :: !st
  let name = "NestedIf", check
end

(** -------------------- Checks rules: Nesting match >= 3 levels ------------- *)
module NestedMatch : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using let statements or helper methods / rethinking logic"
  let violation = "using nested match statements more than three layers deep"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      (* Layer one *)
      | Pexp_match (_, cs) ->
        let matches_three = List.map (fun (c: Parsetree.case) -> c.pc_rhs |> skip_seq_let) cs |>
                            (* Grab the second layer *)
                            List.map (fun (e: Parsetree.expression) ->
                                match e.pexp_desc with
                                | Pexp_match (_, cs) ->
                                  List.map (fun (c: Parsetree.case) -> c.pc_rhs |> skip_seq_let) cs
                                | _ -> []
                              ) |> List.flatten |>
                            (* Grab the third layer *)
                            List.exists (fun (e: Parsetree.expression) ->
                                match e.pexp_desc with
                                | Pexp_match _ -> true
                                | _ -> false ) in
        if matches_three then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "NestedMatch", check
end

(** ------------ Checks rules: if _ then/else true | false  ------------------ *)
module IfReturnsLit : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "returning just the condition (+ some tweaks)"
  let violation = "using an if statement to return `true | false` literally"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_, bthen, Some belse) ->
        if (bthen =| "true" && belse =| "false") ||
           (bthen =| "false" && belse =| "true") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfReturnsLit", check
end

(** ------------ Checks rules: if cond then cond | if cond then _ else cond -- *)
module IfCondThenCond : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "returning just the condition or simplifying further"
  let violation = "returning the condition of an if statement on success and a boolean literal otherwise"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, bthen, Some belse) ->
        if e_eq cond bthen && (belse =| "false" || belse =| "true") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfCondThenCond", check
end


(** ------------ Checks rules: if not cond then x else y --------------------- *)
module IfNotCond : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "swapping the then and else branches of the if statement"
  let violation = "checking negation in the if condition"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, _, _) ->
        begin match cond.pexp_desc with
          | Pexp_apply (func, [_]) -> if func =~ "not" then
              st := Hint.mk_hint location source fix violation :: !st
          | _ -> ()
        end
      | _ -> ()
    end
  let name = "IfNotCond", check
end

(** ------------ Checks rules: if x then true else y ------------------------- *)
module IfToOr : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `||`"
  let violation = "overly verbose if statement that can be simplified"

  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (belse =| "true") &&
           not (belse =| "false") &&
           bthen =| "true" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end

  let name = "IfToOr", check
end

(** ------------ Checks rules: if x then y else false ------------------------ *)
module IfToAnd : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `&&`"
  let violation = "overly verbose if statement that can be simplified"

  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (cond, bthen, Some belse) ->
        if not (bthen =| "true") &&
           not (bthen =| "false") &&
           belse =| "false" &&
           e_neq cond bthen
        then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToAnd", check
end

(** ------------ Checks rules: if x then false else y ------------------------ *)
module IfToAndInv : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `&&` and `not`"
  let violation = "overly verbose if statement that can be simplified"
  let check st (E {location;source;pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (belse =| "true") && not (belse =| "false")  && bthen =| "false" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToAndInv", check
end

(** ------------ Checks rules: if x then y else true ------------------------ *)
module IfToOrInv : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "rewriting using a boolean operator like `||` and `not`"
  let violation = "overly verbose if statement that can be simplified"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_ifthenelse (_cond, bthen, Some belse) ->
        if not (bthen =| "true") && not (bthen =| "false") && belse =| "true" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "IfToOrInv", check
end

(** ------------ Checks rules: ... || true | true || ... | false || ... | ... || false -- *)
module RedundantOr : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "simplifying further"
  let violation = "Usage of the `||` is redundant"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (appl, [(_, l);(_, r)]) ->
        if appl =~ "||" && (e_eq l r ||
                            l =| "true" ||
                            l =| "false" ||
                            r =| "true" ||
                            r =| "false") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "RedundantOr", check
end

(** ------------ Checks rules: ... && true | true && ... | false && ... | ... && false -- *)
module RedundantAnd : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "simplifying further"
  let violation = "Usage of the `&&` is redundant"
  let check st (E {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pexp_apply (appl, [(_, l);(_, r)]) ->
        if appl =~ "&&" && (e_eq l r ||
                            l =| "true" ||
                            l =| "false" ||
                            r =| "true" ||
                            r =| "false") then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "RedundantAnd", check
end

(** ------------------ Checks rules: (_ = :bool | :bool = _)  ----------------------- *)
module EqBool : EXPRCHECK = struct
  type ctxt = Parsetree.expression_desc Pctxt.pctxt
  let fix = "using the variable itself to represent the value"
  let violation = "using `=` with boolean as a comparison"
  let check st (E {location; source; pattern}: ctxt) = 
    begin match pattern with
      | Pexp_apply (application, [(_,lop); (_,rop)]) ->
        if application =~ "=" && (is_exp_bool lop || is_exp_bool rop) then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
  let name = "EqBool", check
end