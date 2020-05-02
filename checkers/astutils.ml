open Style
open Parsetree

let is_id e id : bool = 
  match e.pexp_desc with 
  | Pexp_ident {txt = Lident i} -> i = id
  | _ -> false

let is_list_literal (e : exp) : bool =
  match e with
  (* Nil case *)
  | Pexp_construct ({txt = Lident "[]"} , _) -> true
  (* Cons case *)
  | Pexp_construct ({txt = Lident "::"}, _) -> true
  | _ -> false

let is_option_lit (e : exp) : bool =
  match e with
  | Pexp_construct ({txt = Lident "None"}, _) (* None case *)
  | Pexp_construct ({txt = Lident "Some"}, _) -> true (* Some case *)
  | _ -> false

let is_true_lit (e: exp) : bool = 
  match e with
  | Pexp_construct ({txt = Lident "true"}, _) -> true
  | _ -> false

let is_false_lit (e: exp) : bool = 
  match e with
  | Pexp_construct ({txt = Lident "false"}, _) -> true
  | _ -> false
  
let is_bool_lit (e: exp) : bool = is_true_lit e || is_false_lit e

