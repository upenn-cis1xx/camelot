open Canonical
  
module type CHECK = sig
  (* The fix for this check *)
  val fix : Hint.fix
  (* The violation associated with this check *)
  val violation : Hint.violation
  (* A method that performs a check, given a lint context, and adds the hint to the list*)
  val check : Hint.hint list ref -> Pctxt.patternctxt -> unit
end
