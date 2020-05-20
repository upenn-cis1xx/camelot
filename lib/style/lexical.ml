open Canonical
open Utils.IOUtils
       
module LineLength : Check.CHECK = struct
  let fix = "indenting to avoid exceeding the line limit"
  let violation = "exceeding the 80 character line limit. Only showing (1) such violation of this kind, although there may be others - fix this and re-run the linter to find them."
    
  let max_line_length = 80
  let seen_line_exceed : bool ref = ref false
      
  let check st (ctxt: Pctxt.patternctxt) =
    if (ctxt.location.col_start > max_line_length || ctxt.location.col_end > max_line_length)
       && not (!seen_line_exceed)
    then
      let raw = read_at_loc ctxt.location in
      st := Hint.mk_hint ctxt.location raw fix violation :: !st;
      seen_line_exceed := true
      
end
