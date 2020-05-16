module Hint = Canonical.Hint
module Trav = Traverse



let store : Hint.hint list ref = ref []

(* Build a list of hints using the patterns *)

let lint_one : (string * Parsetree.structure) -> unit = fun (fname, ast) ->
  let _ = Trav.make_iterator fname |>
          Trav.apply_iterator ast in
  print_endline @@ "linting " ^  fname;       
  List.iter (fun pctxt ->
      List.iter (fun check -> check store pctxt) Checkers.checks
    ) (Trav.patterns ())


let lint : (string * Parsetree.structure) list -> unit =
  List.iter (lint_one)

let hints : unit -> Hint.hint list = fun _ -> !store

