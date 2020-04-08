open Parsetree

(* Warning location *)
type warn_loc = {line: int; startchar: int; endchar: int}

let warn_loc line s e = {line = line; startchar = s; endchar = e}

(* Definition of style guide *)
type rule =
  | BPat of bpat
and bpat =
  | IfReturnsLit    (* if cond then true else false *)
  | IfReturnsLitInv (* if cond then false else true *)
  | IfReturnsCond   (* if cond then cond else _     *)
  | IfCondNeg       (* if not cond then x else y    *)
  | IfReturnsTrue   (* if cond then true else y        *)
  | IfFailFalse     (* if cond then y else false       *)
  | IfSuccFalse     (* if cond then false else y       *)
  | IfFailTrue      (* if cond then y else true        *)

(* Warning location and rule violated *)
type warn = { loc: warn_loc; violation: rule }

type exp = Parsetree.expression_desc
             
type expr =
  | EIfThenElse of exp * exp * exp

let rule_of_warn : warn -> rule = fun { loc=_; violation=v } -> v

let string_of_rule : rule -> string = 
  function
  | BPat IfReturnsLit    -> "if must return a literal value"
  | BPat IfReturnsLitInv -> "if must return a literal value"
  | BPat IfReturnsCond   -> "if must return a literal value"
  | BPat IfCondNeg       -> "'not' in if condition must be simplified"
  | BPat IfReturnsTrue   -> "'if cond then true else x' can be simplifed to 'cond || x'"
  | BPat IfFailFalse     -> "'if cond then x else false' can be simplifed to 'cond && x'"
  | BPat IfSuccFalse     -> "'if cond then false else x' can be simplifed to 'cond && x'"
  | BPat IfFailTrue      -> "'if cond then true else x' can be simplifed to 'cond || x'"

