open Ast_iterator
open Ast_helper
open Asttypes
open Parsetree
open Descent

(**
   Defines a linter iterator for traversing the OCaml AST.
   You can write your own Custom expression handler 
*)
let rec linterator exp_handler = 
  {
    default_iterator with
    expr = expr_iterator exp_handler;
  }

(* Iterator for traversing expressions *)
and expr_iterator (handler: Parsetree.expression -> 'a) (iterator: Ast_iterator.iterator) (expr: Parsetree.expression) : unit =
  let {pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes} = expr in
  handler expr;
  iterator.location iterator pexp_loc;
  iterator.attributes iterator pexp_attributes;
  match pexp_desc with


  (** Simple Expressions - values, constants, functions, matches, f appls *) 
  | Pexp_ident x -> d_ident iterator x
  | Pexp_constant _ -> () 
  | Pexp_let (_recFlag, values, expr) -> d_let iterator values expr
  | Pexp_function (cases) -> d_function iterator cases
  | Pexp_fun (_arg_label, exp_opt, pat, exp) -> d_fun iterator exp_opt pat exp
  | Pexp_apply (exp, labeled_exps) -> d_apply iterator exp labeled_exps
  | Pexp_match (exp, cases) -> d_match iterator exp cases

  (** More Complex Expressions and statements - try, tuple, constructors, records, and more *)
  | Pexp_try (exp, cases) -> d_try iterator exp cases 
  | Pexp_tuple exps -> d_tuple iterator exps
  | Pexp_construct (loc, exp_opt) -> d_construct iterator loc exp_opt
  | Pexp_variant (_lab, exp_opt) -> d_variant iterator exp_opt
  | Pexp_record (labeled_exps, exp_opt) -> d_record iterator labeled_exps exp_opt
  | Pexp_field (exp, loc) -> d_field iterator exp loc
  | Pexp_setfield (exp_src, loc, exp_dst) -> d_setfield iterator exp_src loc exp_dst
  | Pexp_array exps -> d_array iterator exps

  (** Probably the most important for the linter *)
  | Pexp_ifthenelse (test, ethen, else_opt) -> d_ifthenelse iterator test ethen else_opt

  (** Misc *)
  | Pexp_sequence (first, next) -> d_sequence iterator first next
  (** Syntax typically unused in 120: While, For, coerce. Constraint may be of interest *)
  | Pexp_while (test, exp) -> d_while iterator test exp
  | Pexp_for (pat, expA, expB, _, expthen) -> d_for iterator pat expA expB expthen
  | Pexp_constraint (exp, core) -> d_constraint iterator exp core
  | Pexp_coerce (exp, opt_core, core) -> d_coerce iterator exp opt_core core
  | Pexp_send (exp, loc) -> d_send iterator exp loc
  | Pexp_new loc -> d_new iterator loc

  (** Really not that useful in 120 *)
  | Pexp_setinstvar (loc, exp) -> d_setinstvar iterator loc exp
  | Pexp_override labeled_exps -> d_override iterator labeled_exps
  | Pexp_letmodule (loc, module_exp, exp) -> d_letmodule iterator loc module_exp exp
  | Pexp_letexception (ext_con, exp) -> d_letexception iterator ext_con exp
  | Pexp_assert exp -> d_assert iterator exp

  (** Honestly, I have no idea what the syntax here means *)
  | Pexp_lazy e -> d_lazy iterator e
  | Pexp_poly (exp, opt_core) -> d_poly iterator exp opt_core
  | Pexp_object class_struct -> d_object iterator class_struct
  | Pexp_newtype (loc, exp) -> d_newtype iterator loc exp
  | Pexp_pack mod_expr -> d_pack iterator mod_expr
  | Pexp_open (open_decl, exp) -> d_open iterator open_decl exp
  | Pexp_letop letop -> d_letop iterator letop
  (** Might be useful in the future if you want to tweak the OCaml that students are writing *)
  | Pexp_extension ext -> d_extension iterator ext
  | Pexp_unreachable -> ()