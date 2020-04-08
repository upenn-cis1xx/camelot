open Report

module type CHECK = sig 
  val check : Report.lctxt -> Report.warn option
end
