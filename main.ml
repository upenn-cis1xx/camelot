(** Entry point for the OCaml linter.

    Parses command line args, and runs the linter

*)
open Lexing
open Parse
open Parsetree
open Linter
open Traverse

let lint_dir: string ref = ref "./" (* lint the current directory if none provided *)

(* The spec we'll be using to format command line arguments *)
let spec =
  let open Arg in
  align [
    "-d", Set_string lint_dir, 
    "Invoke the linter on the provided directory, defaulting to the current directory, non re"
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

let files_in_dir dirname = 
  let open Sys in
  if not (file_exists dirname && is_directory dirname) 
  then fail @@ dirname ^ " doesn't exist or isn't a directory!";
  readdir dirname |> Array.to_list |> List.map (fun file -> dirname ^ file)

let usage_msg =
  "invoke with -d <dir_name> to specify a directory to lint, or just run the program with default args" 

let parse_sources_in dirname = 
  let open Sys in
  let to_lint = dirname |>
                files_in_dir |> (* grab the files in the directory *)
                List.filter (fun f -> not (is_directory f)) |> (* remove directories *)
                List.filter (fun f -> Filename.check_suffix f ".ml") |> (* only want to lint *.ml files *)
                List.map (lex_src) |> (* Lex the files *)
                List.map (parse_src) (* Parse the files *) in
  to_lint



let () = 
  Arg.parse spec (fun _ -> ()) usage_msg;
  let tolint = parse_sources_in !lint_dir in
  List.iter ( fun (src_name, parsed_tree) ->
      let pattern_finder = Linter.patterns_of_interest src_name in
      let linterator = Traverse.linterator pattern_finder 1 in
      Traverse.apply_iterator linterator parsed_tree ;
      Linter.lint ();
      Linter.hint ()
    ) tolint;



