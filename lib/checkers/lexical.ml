module Ctxt = Canonical.Patternctxt
module Hint = Canonical.Hint
                
module LineLength : Check.CHECK = struct
  let fix = "indenting to avoid exceeding the line limit"
  let violation = "exceeding the 80 character line limit"
  let max_line_length = 80
    
  let check st (ctxt: Ctxt.patternctxt) =
    if (ctxt.location.col_start > max_line_length || ctxt.location.col_end > max_line_length) then
     st := Hint.mk_hint ctxt.location ctxt.source fix violation :: !st
end
