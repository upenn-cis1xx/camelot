
(** Limited notion of expression equality - trees should look the same
    for commonly used expressions eee
*)
let rec exp_eq (el: Parsetree.expression) (er: Parsetree.expression) =
  exp_desc_eq el.pexp_desc er.pexp_desc

and exp_desc_eq (el: Parsetree.expression_desc) (er: Parsetree.expression_desc) =
  match el, er with
  | Pexp_ident {txt = Lident i; _}, Pexp_ident {txt = Lident j; _} -> i = j
  | Pexp_constant c, Pexp_constant d -> const_eq c d
  | Pexp_let (lrec, _lvblist, el'), Pexp_let (rrec, _rvblist, er') ->
    (* Ignore the value bindings ooo *)
    lrec = rrec && exp_eq el' er'
  | Pexp_apply (el,largs), Pexp_apply (er, rargs) ->
    exp_eq el er &&
    List.for_all2 (fun (_, l) (_, r) -> exp_eq l r ) largs rargs
  | Pexp_tuple ls, Pexp_tuple rs -> List.for_all2 (exp_eq) ls rs
  | Pexp_construct ({txt = Lident l; _}, None), Pexp_construct ({txt = Lident r; _}, None) ->
    l = r 
  | Pexp_construct ({txt = Lident l; _}, Some el), Pexp_construct ({txt = Lident r; _}, Some er) ->
    l = r && exp_eq el er 
  | _ -> false

and value_binding_eq (el: Parsetree.value_binding) (er: Parsetree.value_binding) =
  exp_eq el.pvb_expr er.pvb_expr &&
  pat_eq el.pvb_pat er.pvb_pat

and pat_eq (_el: Parsetree.pattern) (_er: Parsetree.pattern) =
  false

and const_eq (el: Parsetree.constant) (er: Parsetree.constant) = el = er
