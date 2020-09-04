open Warnloc


type violation = string
type fix = string

type hint = {
  loc: warn_loc
; raw: string
; fix: fix
; violation : string
}



let line_hint file line content =
  let warn = {file;
              line_start = line;
              line_end = line;
              col_start = 0;
              col_end = 80
             } in
  {loc = warn;
   raw = content;
   fix = "indenting to avoid exceeding the 80 character line limit";
   violation = "exceeding the 80 character line limit"}



let mk_hint loc raw fix violation = {loc; raw; fix; violation}






