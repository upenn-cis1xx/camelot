(** 
   Parser for the json type to an internal type 
*)



(* Internal Config type *)
type config =
  | Flags of flag list

(** Will be extended with an enable flag  *)
and flag =
  | Disable of string


(** Looks for an arthur.json file *)
let from_file : Yojson.Basic.t option =
  try Some (Yojson.Basic.from_file "arthur.json")
  with _ -> None

(** Converts an optional json file to a config *)
let to_config : Yojson.Basic.t option -> config = function
  | None -> Flags []
  | Some config ->  Flags (
      config |>
      Yojson.Basic.Util.member "disabled" |>
      (fun field -> try Yojson.Basic.Util.to_list field with _ -> []) |>
      List.filter_map (fun jstring -> try Some (jstring |> Yojson.Basic.Util.to_string) with _ -> None) |>
      List.map (fun rule -> Disable rule)
    )


(** Pretty Prints the config list *)
let pp_config : config -> unit = function
  | Flags fs ->
    let print_flags _ =
      let rec sep : string list -> string = fun l ->
        match l with
        | [] -> ""
        | [v] -> v
        | h :: t -> h ^ ", " ^ sep t in
      let sof = fs |> List.map (fun (Disable s) -> s) |> sep in
      print_endline "\t[";
      print_endline @@ "\t\t" ^ sof;
      print_endline "\t]"
    in
    
    print_endline "{";
    print_endline "\t";
    print_endline "disabled :";
    print_flags ();
    print_endline "}"
      
    
