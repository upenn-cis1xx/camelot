open Parsetree
open Pprintast
open Location
(* Warning location *)
type warn_loc = { file: string
                ; line_start: int
                ; line_end: int
                ; col_start: int
                ; col_end: int
                }

let warn_loc f ls le cs ce = { file = f 
                             ; line_start = ls
                             ; line_end = le
                             ; col_start = cs
                             ; col_end = ce
                             } 

let warn_loc_of_loc f l : warn_loc =
  let start = l.loc_start in
  let fin = l.loc_end in
  warn_loc f start.pos_lnum
    fin.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (fin.pos_cnum - fin.pos_bol)

(* Definition of style guide *)
type rule =
  | BPat of bpat    (* Boolean patterns  *)
  | MPat of mpat    (* Match Patterns    *)
  | EqPat of epat   (* Equality Patterns *)
  | HofPat of hpat  (* HOF patterns      *)
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
and hpat =
  | MapFxn
  | IterFxn
  | FoldFxn
and custom =
  | Blank

(* Convenience wrappers for Parstree nodes *)
type exp = Parsetree.expression_desc
type pat = Parsetree.pattern_desc
type cases = Parsetree.case list

(* Relevant information to extract from the parsetree 
   wrapped in our constructors
*)

(* Patterns we want to match for and wrap in constructors *)
type pattern = 
  | EqApply of exp * exp
  (* the pattern is the stmt you're declaring, 
     and the exp is the match *)
  | RecDecMatch of pat * exp 
  | Compile_Blank

(* Useful information about the region of code we're working with
   Contains the location information if a warning is neede, the pattern in question,
   and the string source.
*)
type patternctxt = {location: warn_loc; source: string; pattern: pattern }

let mk_pc location source pattern = {location; source; pattern}


type fix = string (* Suggestion to fix *)

(* A hint - what we want after checking a patternctxt *)
type hint = { loc       : warn_loc (* location to hint at *)
            ; raw       : string   (* raw src code *)
            ; fix       : fix      (* the fix *)
            ; violation : rule     (* the rule being violated *)
            }

let mk_hint loc raw fix violation =
  {loc; raw; fix; violation}

let string_of_rule : rule -> string = function
  | Custom _ -> "Overload this yourself"
  | BPat b ->
    begin match b with
      | IfReturnsLit -> "If statement returns true on success and false otherwise"
      | IfReturnsLitInv -> "If statement returns true on failure and false on success"
      | IfReturnsCond -> "If statement returns the condition it checks for on success"
      | IfCondNeg -> "If statement checks using not"
      | IfReturnsTrue -> "If statement returns true on success and var on fail"
      | IfFailFalse -> "If statement returns var on success and false on fail "
      | IfSuccFalse -> "If statement returns false on success and var on fail"
      | IfFailTrue -> "If statement returns var on success and true on fail"
    end
  | MPat m ->
    begin match m with
      | OneArmedMatch -> "Pattern match has only one case"
    end
  | EqPat e ->
    begin match e with
      | EqOption -> "Checking for structural equality with an option literal"
      | EqList -> "Checking equality with list literals"
    end
  | HofPat h ->
    begin match h with
      | MapFxn |IterFxn | FoldFxn -> "Complex code that could use a hof"
    end