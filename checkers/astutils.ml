open Report


let is_list_literal (e : exp) : bool =
  match e with
    (* Nil case *)
    | Pexp_construct ({txt = Lident "[]"} , _) -> true
    (* Cons case *)
    | Pexp_construct ({txt = Lident "::"}, _) -> true
    | _ -> false

let is_option_lit (e : exp) : bool =
  match e with
    (* None case *)
    | Pexp_construct ({txt = Lident "None"}, _) -> true
    (* Some case *)
    | Pexp_construct ({txt = Lident "Some"}, _) -> true
    | _ -> false

