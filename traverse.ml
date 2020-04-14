open Ast_iterator
open Ast_helper
open Asttypes
open Parsetree
open Descent

let rec linter_iterator exp_handler case_handler = 
  {
    default_iterator with
    expr = expr_iterator;
  }

(* Iterator for traversing expressions *)
and expr_iterator (iterator: Ast_iterator.iterator) (expr: Parsetree.expression) : unit =
  let {pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes} = expr in
  iterator.location iterator pexp_loc;
  iterator.attributes iterator pexp_attributes;
  match pexp_desc with
  (* Patterns we care about*)
  | Pexp_ifthenelse (test, bthen, Some belse) -> 
    d_ifthenelse iterator test bthen belse
  | Pexp_sequence (first, next) ->
    d_sequence iterator first next

  (* Patterns we don't care about:
     We are force to pattern match because, we still want to recursively
     lint expressions inside other expressions, even if we don't care about
     the actual enclosing expression. 
     E.g. while (if true then true else false, _).
     We don't care about the while, but we still need lint output for the
     condition.
  *)
  | Pexp_ident x -> d_ident iterator x
  | Pexp_constant _ -> () 
  | Pexp_let (_recFlag, values, expr) -> d_let iterator values expr
  | Pexp_function (cases) -> d_function iterator cases
  | Pexp_fun (_arg_label, exp_opt, pat, exp) -> d_fun iterator exp_opt pat exp
  | Pexp_apply (exp, labeled_exps) -> d_apply iterator exp labeled_exps
  | Pexp_while (test, exp) -> d_while iterator test exp
  | Pexp_for (pat, expA, expB, _, expthen) -> d_for iterator pat expA expB expthen
  | Pexp_constraint (exp, core) -> d_constraint iterator exp core
  | Pexp_coerce (exp, opt_core, core) -> d_coerce iterator exp opt_core core
  | Pexp_send (exp, loc) -> d_send iterator exp
  | Pexp_new (id) -> ()
  | _ -> ()