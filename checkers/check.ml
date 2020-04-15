open Style

module type CHECK = sig 
  val fix : fix
  val check : Style.hint list ref -> Style.patternctxt -> unit
end
