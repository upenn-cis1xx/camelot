open Report

module type CHECK = sig 
  val pattern : pattern
  val fix : fix
  val check : Report.lctxt -> hint option
end
