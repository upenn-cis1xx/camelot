open Report

module type CHECK = sig 
  type lintstate = Report.hint list ref in
val fix : fix
val check : lintstate -> Report.lctxt -> ()
end
