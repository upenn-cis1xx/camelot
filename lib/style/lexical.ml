open Canonical
open Check

(** --------- Checks rules: lines that exceed 80 characters in a given file ------------ *)
module LineLength : LEXICALCHECK = struct

  type ctxt = Pctxt.file Pctxt.pctxt 

  let fix = "indenting to avoid exceeding the line limit"

  let violation = "exceeding the 80 character line limit. Only showing (1) such violation of this kind, although there may be others - fix this and re-run the linter to find them."

  let check st (L {source; pattern = Pctxt.F chan}: ctxt) =
    let filestream : (int * string) Stream.t =
      (* Stream.from 0 indexes file lines, but line numbers start at 1. Have to increment so that the line numbers are consistent with editors :) *)
      Stream.from
        (fun line -> try (Some (line + 1, input_line chan))
          with End_of_file -> None
        ) in

    Stream.iter (fun (line_no, line) ->
        if String.length line > 80 then
          st := Hint.line_hint source line_no line :: !st
      ) filestream

  let name = "LineLength", check

end
