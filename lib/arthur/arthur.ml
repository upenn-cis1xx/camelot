type t = Arthur_parse.arthur


let lint_config_file : string ref = ref "arthur.json"



let print_config : t -> unit = fun v ->
  print_string @@ Arthur_parse.pp_arthur v
    
let parse : unit -> t = fun _ ->
  Arthur_parse.json_to_arthur (Arthur_parse.from_file !lint_config_file)

let extract : t -> (string * 'a) list -> (string * 'a) list = fun c rules ->
  match c with
  | Arthur (Global (Disable toDisable), _) ->
    List.filter
      (fun (name,_) ->  not (List.exists (fun dis -> dis = name) toDisable)  )
      rules

let refine : t -> string -> (string * 'a) list -> 'a list = fun config func rules ->
  let open Arthur_parse in
  let config_has_rule (fs: func list) = List.exists (fun (Func (l,_)) -> l = func) fs in
  let rule_for_func f (fs: func list) =
    let return = List.find_map
        (fun (Func (l, Disable ls)) -> if l = f then Some ls else None ) fs in
    match return with
    | None -> failwith "literally impossible"
    | Some l -> l
  in 
  let all = List.map snd rules in
  match config with
  | Arthur (_, fs) ->
    if config_has_rule fs then
      let toDisable = rule_for_func func fs in
      List.filter
        (fun (name, _) -> not (List.exists (fun dis -> dis = name) toDisable ) )
        rules |> List.map snd
    else
      (* If the config has no rule - only globals apply *)
      all
let default = Arthur_parse.default
