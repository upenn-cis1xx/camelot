open Parsetree

(* Warning location *)
type warn_loc = {line: int; startchar: int; endchar: int}

let warn_loc line s e = {line = line; startchar = s; endchar = e}

(* Definition of style guide *)
type rule =
  | BPat of bpat
and bpat =
  | IfReturnsLit    (* If cond then true else false *)
  | IfReturnsLitInv (* If cond then false else true *)
  | IfReturnsCond   (* If cond then cond else _     *)
  | IfCondNeg       (* If not cond then x else y    *)
  | IfReturnsTrue   (* If x then true else y        *)
  | IfFailFalse     (* If x then y else false       *)
  | IfSuccFalse     (* If x then false else y       *)
  | IfFailTrue      (* If x then y else true        *)

(* Warning location and rule violated *)
type warn = {loc: warn_loc; violation:rule}

type hint =
  | Fix


(* Warning and suggestion for fix *)
type check = warn * hint


(* Convenience wrapper for Parstree expressions *)
type exp = Parsetree.expression_desc

(* 
  Useful for separating the actions of the ParseTree and the mappers open recursion
  from the linting work
 *)
             
type expr =
  | EIfThenElse of exp * exp * exp
  | Compile_Blank


let string_of_warn : warn -> string = function
  | {loc = _; violation = BPat p}->
    match p with
    | IfReturnsLit -> "If cond then true else false"
    | IfReturnsLitInv -> "If cond then false else true"
    | IfReturnsCond -> "If cond then cond else y"
    | IfCondNeg -> "if not cond then x else y"
    | IfReturnsTrue -> "If x then true else y"
    | IfFailFalse -> "If x then y else false"
    | IfSuccFalse -> "If x then false else y"
    | IfFailTrue -> "If x then y else true"
