open Parsetree
open Ast_iterator

(* Iterates through the ast structure, applying the given iterator *)
let apply_iterator (iter: Ast_iterator.iterator) (structure: Parsetree.structure) : unit = 
  iter.structure iter structure