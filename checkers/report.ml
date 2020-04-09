open Parsetree

(* Warning location *)
type warn_loc = { line_start: int
                ; line_end: int
                ; col_start: int
                ; col_end: int
                }

let warn_loc ls le cs ce = { line_start = ls
                           ; line_end = le
                           ; col_start = cs
                           ; col_end = ce
                           } 

(* Definition of style guide *)
type rule =
  | BPat of bpat    (* Boolean patterns *)
  | MPat of mpat    (* Match Patterns   *)
and bpat =
  | IfReturnsLit    (* If cond then true else false *)
  | IfReturnsLitInv (* If cond then false else true *)
  | IfReturnsCond   (* If cond then cond else _     *)
  | IfCondNeg       (* If not cond then x else y    *)
  | IfReturnsTrue   (* If x then true else y        *)
  | IfFailFalse     (* If x then y else false       *)
  | IfSuccFalse     (* If x then false else y       *)
  | IfFailTrue      (* If x then y else true        *)
and mpat = 
  | OneArmedMatch   (* match x with | <> -> *)

(* Warning location and rule violated *)
type warn = {loc: warn_loc; violation:rule}

(* Convenience wrappers for Parstree nodes *)
type exp = Parsetree.expression_desc
type cases = Parsetree.case list
    
type code =
  | EIfThenElse of exp * exp * exp
  | PPatternMatch of exp * cases
  | Compile_Blank

type lctxt = {location: warn_loc; code: code}


(* 
  Useful for separating the actions of the ParseTree and the mappers open recursion
  from the linting work
 *)

let string_of_rule : rule -> string = function
  | BPat b ->
    begin match b with
    | IfReturnsLit -> "If cond then true else false"
    | IfReturnsLitInv -> "If cond then false else true"
    | IfReturnsCond -> "If cond then cond else y"
    | IfCondNeg -> "if not cond then x else y"
    | IfReturnsTrue -> "If x then true else y"
    | IfFailFalse -> "If x then y else false"
    | IfSuccFalse -> "If x then false else y"
    | IfFailTrue -> "If x then y else true"
    end
  | MPat m ->
    begin match m with
      | OneArmedMatch -> "begin match <> with | _ -> <> end is a one armed match"
    end
    


let string_of_warn : warn -> string = function
  | {loc; violation} ->
    (string_of_rule violation)
    ^ "\n\t" ^
    (
      (if loc.line_start = loc.line_end then "line: " ^ (string_of_int loc.line_start)
       else "lines: " ^ (string_of_int loc.line_start) ^ "-" ^ (string_of_int loc.line_end)
      )
    ) ^ ", " ^
    (
      "columns: " ^ (string_of_int loc.col_start) ^ "-" ^ (string_of_int loc.col_end)
    )    
