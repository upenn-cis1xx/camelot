open Canonical
open Traverse

let store : Hint.hint list ref = ref []


let line_length_lint : string -> unit = fun file ->
  let (_in_pid, out_pid) = Unix.pipe () in
  (* Use create process for windows compatibility
     Fork a process that runs awk
 *)
  let pid = Unix.create_process "awk"
      (Array.of_list ["length>79{print FNR}"; file])
      Unix.stdin
      out_pid
      Unix.stderr
  in

  match pid with
  | 0 ->
    (* read from the out channel *)
    let chan = Unix.in_channel_of_descr out_pid in
    (* O fuck me why am I writing imperative code in OCaml *)
    let lines = ref [] in
    let _ =
      try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines in
    let hints = !lines |>
                List.filter_map int_of_string_opt |>
                List.map (fun l -> Hint.line_hint file l) in
    store := hints @ !store
  | -1 -> (* Error : just return *)
    ()
  | _ -> (* If you're not the child or error lmao - waitpid for windows compatibility *)
    let _ = Unix.waitpid [] (-1) in
    ()

(* Build a list of hints using the patterns *)

let lint : (string * Parsetree.structure) list -> unit =
  List.iter (fun (file, ast) ->
      Iter.make_linterator store file |>
      Iter.apply_iterator ast
    )

let hints : unit -> Hint.hint list = fun _ -> !store

