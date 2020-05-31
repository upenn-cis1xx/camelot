open Canonical
open Traverse

let store : Hint.hint list ref = ref []


let line_length_lint : string -> unit = fun file ->
  let chan = open_in file in
  let lref : int ref = ref 1 in
  try
    while true; do
      let line = input_line chan in
      (if (String.length line > 80) then store := Hint.line_hint file !lref line :: !store;);
      incr lref
    done; ()
  with End_of_file ->
    close_in chan; ()
        
(* Build a list of hints using the patterns *)

let lint : (string * Parsetree.structure) list -> unit =
  List.iter (fun (file, ast) ->
      line_length_lint file;
      Iter.make_linterator store file |>
      Iter.apply_iterator ast
    )

let hints : unit -> Hint.hint list = fun _ -> !store

