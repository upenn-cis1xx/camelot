open Parsetree
  
type exp = Parsetree.expression
             
let is_id e id : bool = 
  match e.pexp_desc with 
  | Pexp_ident {txt = Lident i;_} -> i = id
  | _ -> false

let (=~) : exp -> string -> bool = fun e x -> is_id e x

let is_construct (e : exp) id : bool =
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident s;_}, _) -> id = s
  | _ -> false

let (=|) : exp -> string -> bool = fun e x -> is_construct e x


let is_list_lit : exp -> bool = fun e ->
  e =| "::" || e =| "[]"

let is_option_lit : exp -> bool = fun e ->
  e =| "Some" || e =| "None"

let is_pat_constr (case: Parsetree.case) lident_name =
  begin match case.pc_lhs.ppat_desc with 
    | Ppat_construct ({txt = Lident l; loc = _}, _) -> l = lident_name
    | _ -> false
  end

let is_pat_const (case: Parsetree.case) =
  begin match case.pc_lhs.ppat_desc with 
    | Ppat_constant _ -> true
    | _ -> false
  end