open Canonical
open Utils.IOUtils
open Check



module LineLength : EXPRCHECK = struct

  type ctxt = Parsetree.expression_desc Pctxt.pctxt 
  let fix = "indenting to avoid exceeding the line limit"
  let violation = "exceeding the 80 character line limit. Only showing (1) such violation of this kind, although there may be others - fix this and re-run the linter to find them."
    
  let max_line_length = 80
  let seen_line_exceed : bool ref = ref false
      
  let check st (E ctxt: ctxt) =
    if (ctxt.location.col_start > max_line_length || ctxt.location.col_end > max_line_length)
       && not (!seen_line_exceed)
    then
      let raw = code_at_loc ctxt.location ctxt.source in
      st := Hint.mk_hint ctxt.location raw fix violation :: !st;
      seen_line_exceed := true
      
end
