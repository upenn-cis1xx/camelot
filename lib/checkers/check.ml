
module H = Canonical.Hint
module C = Canonical.Patternctxt

module type CHECK = sig
  (* The fix for this check *)
  val fix : H.fix
  (* The violation associated with this check *)
  val violation : H.violation
  (* A method that performs a check, given a lint context, and adds the hint to the list*)
  val check : H.hint list ref -> C.patternctxt -> unit
end
