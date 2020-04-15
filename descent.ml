(* Utility file providing readable helpers to enable recursive AST traversal via the use
   of AST iterators *)

open Ast_iterator
open Parsetree
open Location

let iter_loc iterator {loc; txt = _} = iterator.location iterator loc
let iter_opt f x = match x with | None -> () 
                                | Some v -> f v 
let iter_fst f (t, _) = f t
let iter_snd f (_, t) = f t
let iter_pair fl fr (l, r) = fl l; fr r
let iter_e iterator e = iterator.expr iterator e
let d_ident  = iter_loc

let d_let iterator values expr =
  List.iter (iterator.value_binding iterator) values;
  iterator.expr iterator expr

let d_function iterator cases = iterator.cases iterator cases

let d_fun iterator exp_opt pat exp =
  iter_opt (iterator.expr iterator) exp_opt;
  iterator.pat iterator pat;
  iterator.expr iterator exp

let d_apply iterator exp labels =
  iterator.expr iterator exp;
  List.iter (iter_snd (iterator.expr iterator)) labels

let d_match iterator exp cases =
  iterator.expr iterator exp;
  iterator.cases iterator cases

let d_try iterator exp cases =
  iterator.expr iterator exp;
  iterator.cases iterator cases

let d_tuple iterator exps =
  List.iter (iterator.expr iterator ) exps

let d_construct iterator loc exp_opt =
  iter_loc iterator loc;
  iter_opt (iterator.expr iterator) exp_opt

let d_variant iterator exp_opt =
  iter_opt (iterator.expr iterator) exp_opt

let d_record iterator labeled_exps exp_opt =
  List.iter (iter_pair (iter_loc iterator) (iterator.expr iterator)) labeled_exps;
  iter_opt (iterator.expr iterator) exp_opt

let d_field iterator exp loc =
  iterator.expr iterator exp;
  iter_loc iterator loc

let d_setfield iterator src loc dst =
  iterator.expr iterator src;
  iter_loc iterator loc;
  iterator.expr iterator dst

let d_array iterator exps = 
  List.iter (iterator.expr iterator) exps

let d_ifthenelse iterator test bthen else_opt = 
  iterator.expr iterator test;
  iterator.expr iterator bthen;
  iter_opt (iterator.expr iterator) else_opt


let d_sequence iterator first next = 
  iterator.expr iterator first;
  iterator.expr iterator next

let d_while iterator test exp = 
  iterator.expr iterator test;
  iterator.expr iterator exp

let d_for iterator pat ea eb ethen =
  iterator.pat iterator pat;
  iterator.expr iterator ea;
  iterator.expr iterator eb;
  iterator.expr iterator ethen

let d_constraint iterator exp core = 
  iterator.expr iterator exp;
  iterator.typ iterator core

let d_coerce iterator exp opt_core core =
  iterator.expr iterator exp;
  (match opt_core with | None -> ()
                       | Some t -> iterator.typ iterator t );
  iterator.typ iterator core

let d_send iterator exp loc = 
  iterator.expr iterator exp;
  iter_loc iterator loc

let d_new iterator loc =
  iter_loc iterator loc

let d_setinstvar iterator loc exp =
  iter_loc iterator loc;
  iterator.expr iterator exp

let d_override iterator labeled_exps =
  List.iter (iter_pair (iter_loc iterator) (iterator.expr iterator)) labeled_exps

let d_letmodule iterator loc module_exp exp =
  iter_loc iterator loc;
  iterator.module_expr iterator module_exp;
  iterator.expr iterator exp

let d_letexception iterator ext_con exp =
  iterator.extension_constructor iterator ext_con;
  iterator.expr iterator exp

let d_assert iterator exp =
  iterator.expr iterator exp

let d_lazy iterator exp =
  iterator.expr iterator exp 

let d_poly iterator exp opt_core =
  iterator.expr iterator exp;
  iter_opt (iterator.typ iterator) opt_core

let d_object iterator class_struct =
  iterator.class_structure iterator class_struct

let d_newtype iterator loc exp = 
  iter_loc iterator loc;
  iterator.expr iterator exp

let d_pack iterator mod_expr = 
  iterator.module_expr iterator mod_expr

let d_open iterator open_decl exp =
  iterator.open_declaration iterator open_decl;
  iterator.expr iterator exp

let d_letop iterator {let_; ands; body} =
  iterator.binding_op iterator let_;
  List.iter (iterator.binding_op iterator) ands;
  iterator.expr iterator body

let d_extension iterator ext = iterator.extension iterator ext