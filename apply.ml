open Parsetree
open Ast_iterator

(* Iterates through the ast structure, applying the given iterator *)
let apply_iterator (iter: Ast_iterator.iterator) (structure: Parsetree.structure) : unit = 
  print_endline "applied iterator";
  iter.structure iter structure