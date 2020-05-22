open Ast_iterator
open Descent
open Find



(** Iterates through the ast structure, applying the given iterator *)
let apply_iterator (structure: Parsetree.structure) (iter: Ast_iterator.iterator) : unit = 
  iter.structure iter structure


(**
   Defines a linter iterator for traversing the OCaml AST.
   You can write your own Custom expression handler 
*)
let rec linterator exp_handler structitem_handler = 
  {
    default_iterator with
    expr = expr_iterator exp_handler;
    structure_item = structure_item_iterator structitem_handler
    
  }

(* Iterator for traversing expressions *)
and expr_iterator (handler: Parsetree.expression -> 'a) (iterator: Ast_iterator.iterator) (expr: Parsetree.expression) : unit =
  handler expr;
  E.iter iterator expr

(* Iterator for traversing structure_items (top level modules declarations) *)
and structure_item_iterator (handler: Parsetree.structure_item -> 'a) (iterator: Ast_iterator.iterator) (structure_item: Parsetree.structure_item) : unit =
  handler structure_item;
  M.iter_structure_item iterator structure_item



(** Given a list ref and a filename, produce an iterator that runs the pass_checks method at each expression node in the
    OCaml ast. This will mutate the store by appending new hints the checkers find as they analyse the source code.
*)
let make_linterator = fun store fname -> 
  linterator (pass_exprs store fname) (pass_structures store fname)
