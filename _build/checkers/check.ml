open Report
module type CHECK = sig 
  val check : expr -> Report.warn option
end
