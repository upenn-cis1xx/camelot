open Canonical
  
module type CHECK = sig
  type ctxt
  (* The fix for this check *)
  val fix : Hint.fix
  (* The violation associated with this check *)
  val violation : Hint.violation
  (* A method that performs a check, given a lint context, and adds the hint to the list*)
  val check : Hint.hint list ref -> ctxt -> unit
  (* Exposes the name and checker method for convenience. *)
  val name : string * (Hint.hint list ref -> ctxt -> unit)
end

module type EXPRCHECK = CHECK with type ctxt = Parsetree.expression_desc Pctxt.pctxt

module type STRUCTURECHECK = CHECK with type ctxt = Parsetree.structure_item_desc Pctxt.pctxt
  
