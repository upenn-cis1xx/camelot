open Find

module Pat = Canonical.Patternctxt

let store : Pat.patternctxt list ref = ref []

(* Iterates through the ast structure, applying the given iterator *)
let apply_iterator (structure: Parsetree.structure) (iter: Ast_iterator.iterator) : unit = 
  iter.structure iter structure


let make_iterator = fun fname -> 
	Iterator.linterator (find_exprs store fname)

let patterns : unit -> Pat.patternctxt list = fun _ -> !store
