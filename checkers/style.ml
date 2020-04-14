open Parsetree
open Pprintast
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

let warn_loc_of_loc l : warn_loc =
  let start = l.loc_start in
  let fin = l.loc_end in
  warn_loc start.pos_lnum
    fin.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (fin.pos_cnum - fin.pos_bol) in

(* Definition of style guide *)
type rule =
  | BPat of bpat    (* Boolean patterns *)
  | MPat of mpat    (* Match Patterns   *)
  | EqPat of epat
  | Custom of custom (* Extensible rule *)
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
and epat =
  | EqOption
  | EqList
and custom =
  | Blank

(* Convenience wrappers for Parstree nodes *)
type exp = Parsetree.expression_desc
type cases = Parsetree.case list

(* Relevant information to extract from the parsetree 
   wrapped in our constructors
*)

type pattern = 
  | EqApply of exp * exp

type codecontext = {location: warn_loc}


