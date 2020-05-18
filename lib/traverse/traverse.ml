open Find

module Pat = Canonical.Patternctxt


(* Iterates through the ast structure, applying the given iterator *)
let apply_iterator (structure: Parsetree.structure) (iter: Ast_iterator.iterator) : unit = 
  iter.structure iter structure


let make_linterator = fun store fname -> 
	Iterator.linterator (pass_checks store fname)

