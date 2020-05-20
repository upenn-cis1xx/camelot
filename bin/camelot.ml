(** Entry point for the OCaml linter.

    Parses command line args, and runs the linter

*)

open Canonical
open Report
    
let lint_dir : string ref = ref "./" (* lint the current directory if none provided *)
let recurse : bool ref = ref false (* Do not recurse the directory by default *)
let lint_file : string option ref = ref None (*  lint a given file*)
let show_type : (Hint.hint list -> unit) ref = ref Report.Display.student_display (* default to showing hints for students *)
(* The spec we'll be using to format command line arguments *)


let set_display_type : string -> unit = fun s ->
  match s with
  | "ta" -> show_type := Display.ta_display
  | "gradescope" -> show_type := Display.gradescope_display
  | _ -> show_type := Display.student_display

let set_lint_file : string -> unit = fun s ->
  let exist = try
      let _ = open_in s in
      Some s
    with Sys_error _ -> None in
  lint_file := exist

let spec =
  let open Arg in
  align [
    "-r", Set recurse, 
    "\tIf calling on a directory using -d, recurse into its subdirectories"
  ;  "-d", Set_string lint_dir, 
    "Invoke the linter on the provided directory, defaulting to the current directory, non re"
  ; "-show", String set_display_type,
    "Make the linter output display for either ta's | students | gradescope"
  ; "-f", String set_lint_file,
    "Invoke the linter on a single file"
  ] 


let fail msg = prerr_endline msg; exit 1

let safe_open src =
  try src, open_in src 
  with Sys_error msg -> fail msg

let lex_src file =
  let src, f = safe_open file in
  src, Lexing.from_channel f

let parse_src (src, lexbuf) =
  src, Parse.implementation lexbuf

let sanitize_dir d =
    if d.[String.length d - 1] = '/' then d 
    else d ^ "/"

let files_in_dir dirname = 
  let open Sys in
  let dir = sanitize_dir dirname in
  if not (file_exists dir && is_directory dir) 
  then fail @@ dir ^ " doesn't exist or isn't a directory!";
  readdir dir |> Array.to_list |> List.map (fun file -> dir ^ file)

let usage_msg =
  "invoke with -r (only works if -d is set too) to recurse into subdirectories\n" ^
  "invoke with -d <dir_name> to specify a directory to lint, or just run the program with default args\n" ^
  "invoke with -show <student | ta | gradescope> to select the display type - usually ta's want a briefer summary\n" ^
  "invoke with -f <.ml filename> to lint a particular file"

let rec parse_sources_in dirname : (string * Parsetree.structure) list = 
  let open Sys in
  let files_in_dir = (match !lint_file with
      | Some f -> [ f ]
      | None -> dirname |> files_in_dir
    ) in (* Prefer linting single files *) 
  let children : (string * Parsetree.structure) list = 
    files_in_dir |>
    List.filter (fun f -> not (is_directory f)) |> (* remove directories *)
    List.filter (fun f -> Filename.check_suffix f ".ml") |> (* only want to lint *.ml files *)
    List.map (lex_src) |> (* Lex the files *)
    List.map (parse_src) (* Parse the files *) in
  let recursed : (string * Parsetree.structure) list = (* recurse on subdirectories *)
    if !recurse then
      List.concat (List.map (fun d ->
        if is_directory d 
        then parse_sources_in d
        else []
      ) files_in_dir)
    else [] in
  children @ recursed

let () = 
  Arg.parse spec (fun _ -> ()) usage_msg;
  (* Lint the files in the lint directory *)
  parse_sources_in !lint_dir |> Linter.lint;
  (* Display the hints *)
  Linter.hints () |> !show_type
