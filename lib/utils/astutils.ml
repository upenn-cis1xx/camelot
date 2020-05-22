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

let rec skip_seq_let (e: Parsetree.expression) : Parsetree.expression =
  match e.pexp_desc with
  | Pexp_sequence (_, e') -> skip_seq_let e'
  | Pexp_let (_, _, e') -> skip_seq_let e'
  | _ -> e

let get_branches (e: Parsetree.expression) : (Parsetree.expression * Parsetree.expression) option =
  let e = skip_seq_let e in
  match e.pexp_desc with
  | Pexp_ifthenelse(_, bthen, Some belse) -> Some (bthen, belse)
  | _ -> None


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

let are_idents_same (el: Parsetree.expression) (er: Parsetree.expression) =
  match el.pexp_desc, er.pexp_desc with
  | Pexp_ident {txt = Lident i; _} , Pexp_ident {txt = Lident j; _} -> i = j
  | _ -> false

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

let ident_of_let (pat: Parsetree.value_binding) : string =
  match pat.pvb_pat.ppat_desc with
  | Ppat_var {txt = i; loc = _} -> i
  | _ -> ""


let binding_of_lcase (case: Parsetree.case) : string =
  begin match case.pc_lhs.ppat_desc with
  | Ppat_construct ({txt = Lident "::"; loc = _}, Some bound) ->
   begin match bound.ppat_desc with
    | Ppat_tuple [_; tail] ->
      begin match tail.ppat_desc with
        | Ppat_var {txt = t; loc = _} -> t
        | _ -> ""
      end
    | _ -> ""
   end
    | _ -> ""
  end

let uses_func_recursively_list (case: Parsetree.case) func_name tail_binding : bool =
  begin match case.pc_rhs.pexp_desc with
  | Pexp_construct ({txt = Lident "::"; loc = _},
                    Some bound) ->
    begin match bound.pexp_desc with
      | Pexp_tuple ([_; tl]) ->
        begin match tl.pexp_desc with
          | Pexp_apply (func, args) ->
            func =~ func_name &&
            List.exists (fun (_, arg) ->  arg =~ tail_binding) args
          | _ -> false
        end
      | _ -> false
    end
  | _ -> false
  end

let body_of_fun (pat: Parsetree.value_binding) : Parsetree.expression =
  let skipped = skip_seq_let pat.pvb_expr in
  begin match skipped.pexp_desc with
    | Pexp_fun (_, _, _, e) -> skip_seq_let e
    | _ -> skipped
  end
  
