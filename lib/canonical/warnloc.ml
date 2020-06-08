(* Defines the internal warning location type for error reporting *)

open Location

(* Warning location *)
type warn_loc = { file: string
                ; line_start: int
                ; line_end: int
                ; col_start: int
                ; col_end: int
                }

let warn_loc f ls le cs ce = { file = f 
                             ; line_start = ls
                             ; line_end = le
                             ; col_start = cs
                             ; col_end = ce
                             } 

let warn_loc_of_loc f (l: Location.t) : warn_loc =
  let start = l.loc_start in
  let fin = l.loc_end in
  warn_loc f start.pos_lnum
    fin.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (fin.pos_cnum - fin.pos_bol)

let no_loc f = {file = f; line_start = 0; line_end = 0; col_start = 0; col_end = 0}
