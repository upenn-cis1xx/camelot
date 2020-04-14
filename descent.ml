open Ast_iterator
open Parsetree
open Location

let iter_loc iterator {loc; txt = _} = iterator.location iterator loc
let iter_opt f x = match x with | None -> () 
                                | Some v -> f v 
let iter_snd f (_, t) = f t
let d_ident  = iter_loc

let d_let iterator values expr =
  List.iter (iterator.value_binding iterator) values;
  iterator.expr iterator expr

let d_function iterator cases = iterator.cases iterator cases

let d_apply iterator exp labels =
  iterator.expr iterator exp;
  List.iter (iter_snd (iterator.expr iterator)) labels

let d_fun iterator exp_opt pat exp =
  iter_opt (iterator.expr iterator) exp_opt;
  iterator.pat iterator pat;
  iterator.expr iterator exp

let d_ifthenelse iterator test bthen belse = 
  iterator.expr iterator test;
  iterator.expr iterator bthen;
  iterator.expr iterator belse

let d_ifthen iterator test bthen =
  iterator.expr iterator test;
  iterator.expr iterator bthen

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


let d_send iterator exp = iterator.expr iterator exp
