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

let is_exp_const : exp -> bool = fun e ->
  match e.pexp_desc with
  | Pexp_constant _ -> true
  | _ -> false

let is_exp_id : exp -> bool = fun e ->
  match e.pexp_desc with
  | Pexp_ident _ -> true
  | _ ->  false

let is_singleton_list : exp -> bool = fun e ->
  begin match e.pexp_desc with
  | Pexp_construct ({txt = Lident "::";_}, Some cons) ->
    begin match cons.pexp_desc with
      | Pexp_tuple [e_id; e_empty] ->
        (is_exp_const e_id || is_exp_id e_id) && e_empty =| "[]"
      | _ -> false
    end
  | _ -> false
  end
  

let is_option_lit : exp -> bool = fun e ->
  e =| "Some" || e =| "None"


let is_pat_constr (pat: Parsetree.pattern) lident_name =
  match pat.ppat_desc with
  | Ppat_construct ({txt = Lident l; loc = _}, _) -> l = lident_name
  | _ -> false

let is_pat_tuple (pat: Parsetree.pattern) : bool =
  match pat.ppat_desc with
  | Ppat_tuple _ -> true
  | _ -> false

let is_pat_record (pat: Parsetree.pattern) : bool =
  match pat.ppat_desc with
  | Ppat_record _ -> true
  |  _ -> false

let is_case_constr (case: Parsetree.case) = is_pat_constr case.pc_lhs

let is_case_const (case: Parsetree.case) =
  begin match case.pc_lhs.ppat_desc with 
    | Ppat_constant _ -> true
    | _ -> false
  end
