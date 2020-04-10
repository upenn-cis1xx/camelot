open Report

module type CHECK = sig 
  val fix : fix
  val check : Report.lctxt -> hint option
end
