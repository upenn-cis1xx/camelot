open Parsetree
open Pprintast
open Style
open ANSITerminal

let string_of_warn_loc : Style.warn_loc -> string =
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




let string_of_hint : Style.hint -> string =
  fun {loc; raw; fix; violation} ->
  string_of_warn_loc loc ^ "\n" ^
  "Warning:\n\t" ^ string_of_rule violation ^ "\n" ^
  "Pattern Found:\n\t" ^ raw ^ "\n" ^
  "Consider:\n\t" ^ fix ^ "\n\n"


let print_hint : Style.hint -> unit = fun {loc; raw; fix; violation} ->
  let sep = [ANSITerminal.cyan] in
  let pat = [ANSITerminal.magenta] in
  let warn = [ANSITerminal.yellow] in
  let sugg = [ANSITerminal.green; ANSITerminal.Bold] in
  let m_warn, m_rule = string_of_warn_loc loc, string_of_rule violation in
  ANSITerminal.print_string sep
    "(* ------------------------------------------------------------------------ *)\n";
  print_endline @@ m_warn ;
  ANSITerminal.print_string warn "Warning:";
  ANSITerminal.print_string [] ("\n\t" ^ m_rule ^ "\n");
  ANSITerminal.print_string pat ("You wrote:");
  ANSITerminal.print_string [] ("\n\t " ^ raw ^ "\n");
  ANSITerminal.print_string sugg ("Consider:");
  ANSITerminal.print_string [] ("\n\t" ^ fix ^ "\n\n")
