
module Basic = struct
  
  type t = Arthur_parse.config
  let get_config : unit -> t = fun _ ->
    let open Arthur_parse in
    from_file |> to_config

  let arthur_disable : t -> (string * 'a) list -> (string * 'a) list =
    fun (Flags ls) initial ->
    let eq_flag : Arthur_parse.flag -> string -> bool = fun (Disable s1) s2 ->
      s1 = s2 in
    List.filter ( fun (rule, _) ->
        List.exists (fun flag -> eq_flag flag rule ) ls
    ) initial

  let print_config : t -> unit = Arthur_parse.pp_config
end
