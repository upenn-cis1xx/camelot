open Canonical
open Traverse

let store : Hint.hint list ref = ref []

(* Build a list of hints using the patterns *)

let lint : (string * Parsetree.structure) list -> unit =
  List.iter (fun (file, ast) ->
      Iter.make_linterator store file |>
      Iter.apply_iterator ast
    )

let hints : unit -> Hint.hint list = fun _ -> !store

