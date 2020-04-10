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

(* Definition of style guide *)
type rule =
  | BPat of bpat    (* Boolean patterns *)
  | MPat of mpat    (* Match Patterns   *)
  | EqPat of epat
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

(* Convenience wrappers for Parstree nodes *)
type exp = Parsetree.expression_desc
type cases = Parsetree.case list
    
type code =
  | EIfThenElse of exp * exp * exp
  | PPatternMatch of exp * cases
  | EqApply of exp * exp
  | Compile_Blank

type lctxt = {location: warn_loc; code: code; src: string}


(* 
  Useful for separating the actions of the ParseTree and the mappers open recursion
  from the linting work
 *)

type pattern = string (* pattern found against *)
type fix = string (* suggestion to fix pattern *)

type hint = { loc       : warn_loc
            ; violation : rule
            ; pattern   : pattern
            ; fix       : fix
            }


let mk_hint loc violation pattern fix =
  Some {loc; violation; pattern; fix}

let string_of_warn_loc : warn_loc -> string =
  fun {line_start; line_end; col_start; col_end} ->
  "File " ^ !Location.input_name ^ ", " ^ 
  (if line_start = line_end then
     "line " ^ (string_of_int line_start)
   else
     "lines " ^ (string_of_int line_start) ^ "-" ^ (string_of_int line_end)
  ) ^ ", " ^
  (
    "columns: " ^ (string_of_int col_start) ^ "-" ^ (string_of_int col_end)
  )    
                                          

let string_of_rule : rule -> string = function
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

let string_of_hint : hint -> string =
  fun {loc; violation; pattern; fix} ->
  string_of_warn_loc loc ^ "\n" ^
  "Warning:\n\t" ^ string_of_rule violation ^ "\n" ^
  "Pattern Found:\n\t" ^ pattern ^ "\n" ^
  "Consider:\n\t" ^ fix ^ "\n\n"


let print_hint : hint -> unit = fun {loc; violation; pattern; fix} ->
  let sep = [ANSITerminal.cyan] in
  let pat = [ANSITerminal.magenta] in
  let warn = [ANSITerminal.yellow] in
  let sugg = [ANSITerminal.green; ANSITerminal.Bold] in
  let m_warn, m_rule = string_of_warn_loc loc, string_of_rule violation in
  ANSITerminal.print_string sep
    "(* ------------------------------------------------------------------------ *)\n";
  print_endline @@ m_warn ;
  ANSITerminal.print_string warn "Warning:";
  ANSITerminal.print_string [] ("\n\t" ^ m_rule ^ "\n");
  ANSITerminal.print_string pat ("You wrote:");
  ANSITerminal.print_string [] ("\n\t " ^ pattern ^ "\n");
  ANSITerminal.print_string sugg ("Consider:");
  ANSITerminal.print_string [] ("\n\t" ^ fix ^ "\n\n")
