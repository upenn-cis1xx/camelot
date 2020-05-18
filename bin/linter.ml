module Hint = Canonical.Hint
module Trav = Traverse



let store : Hint.hint list ref = ref []

(* Build a list of hints using the patterns *)


let lint : (string * Parsetree.structure) list -> unit =
  List.iter (fun (file, ast) ->
      Trav.make_linterator store file |>
      Trav.apply_iterator ast
    )

let hints : unit -> Hint.hint list = fun _ -> !store

