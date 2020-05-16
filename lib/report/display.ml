
module Warn = Canonical.Warnloc
module Hint = Canonical.Hint

let string_of_warnloc : Warn.warn_loc -> string =
  fun {file; line_start; line_end; col_start; col_end} ->
  "File " ^ file ^ ", " ^ 
  (if line_start = line_end then
     "line " ^ (string_of_int line_start)
   else
     "lines " ^ (string_of_int line_start) ^ "-" ^ (string_of_int line_end)
  ) ^ ", " ^
  (
    "columns: " ^ (string_of_int col_start) ^ "-" ^ (string_of_int col_end)
  )    


(* Utility display methods *)
(* TODO: in the mli file, don't expose these methods  *)
let display_verbose : Hint.hint -> unit =
  fun {loc; raw; fix; violation} ->
  let sep = [ANSITerminal.cyan] in
  let pat = [ANSITerminal.magenta] in
  let warn = [ANSITerminal.yellow] in
  let sugg = [ANSITerminal.green; ANSITerminal.Bold] in
  let m_warn = string_of_warnloc loc in
  ANSITerminal.print_string sep
    "(* ------------------------------------------------------------------------ *)\n";
  print_endline @@ m_warn ;
  ANSITerminal.print_string warn "Warning:";
  ANSITerminal.print_string [] ("\n\t" ^ violation ^ "\n");
  ANSITerminal.print_string pat ("You wrote:");
  ANSITerminal.print_string [] ("\n\t " ^ raw ^ "\n");
  ANSITerminal.print_string sugg ("Consider:");
  ANSITerminal.print_string [] ("\n\t" ^ fix ^ "\n\n")

let display_brief : Hint.hint -> unit =
  fun hint ->
  let warn = [ANSITerminal.yellow] in
  let m_warn = string_of_warnloc hint.loc in
  print_endline @@ m_warn ;
  ANSITerminal.print_string warn "Warning:";
  ANSITerminal.print_string [] (hint.violation ^ "\n\n")


(* Display methods to expose *)
(* TODO: Write an mli file exposing as appropriate *)
    
let student_display : Hint.hint list -> unit =
  List.iter (display_verbose)

let ta_display : Hint.hint list -> unit = fun l ->
  List.iter (display_brief) l;
  let score = "Final score: " ^ string_of_int (Grade.simple l ) ^ " mistakes" in
  let set = [ANSITerminal.green; ANSITerminal.Bold] in
  ANSITerminal.print_string set score
    
