open Canonical

(** The Check interface - fill this out with the kind of check you would like to make. *)
module type CHECK = sig
  
  (** The internal CHECK type - represents the kind of context a checker will need *)
  type ctxt

  (** The fix for this check *)
  val fix : Hint.fix
              
  (** The violation associated with this check *)
  val violation : Hint.violation
                    
  (** A method that performs a check, given a lint context, and adds the hint to the list*)
  val check : Hint.hint list ref -> ctxt -> unit
    
  (** Exposes the name and checker method for convenience. *)
  val name : string * (Hint.hint list ref -> ctxt -> unit)
                      
end

(** Signature for expression checking rules *)
module type EXPRCHECK = CHECK with type ctxt = Parsetree.expression_desc Pctxt.pctxt

(** Signature for structure (top-level) checking rules *)
module type STRUCTURECHECK = CHECK with type ctxt = Parsetree.structure_item_desc Pctxt.pctxt

(** Signature for lexical checking rules *)
module type LEXICALCHECK = CHECK with type ctxt = Pctxt.file Pctxt.pctxt
