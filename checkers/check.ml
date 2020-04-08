open Report
module type CHECK = sig 
  val soc : string
  val check : expr -> Report.warn option
end
