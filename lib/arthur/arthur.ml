
(* Utility for reading/writing a filename into a string *)
let read_file_str (filename: string) : string =
  let ch = open_in filename in
  let filedata = ref "" in
  let rec consume () =
    try 
      let byt = input_char ch in
      filedata := !filedata ^ String.make 1 byt ;
      consume ()
    with End_of_file -> ()
  in
  consume () ;
  !filedata

let write_str_file (source: string) (filename: string) : unit =
  let ch = open_out filename in
  Printf.fprintf ch "%s" source;
  close_out ch

(** Implementing the either monad here *)
module EitherM = struct
  
  type ('a , 'e) t = ('a, 'e) Result.t 

  let return : 'a -> ('a, 'e) t = Result.ok

  let raise : 'e -> ('a, 'e) t = Result.error

  let bind : ('a, 'e) t ->
             ('a -> ('b, 'e) t) -> ('b, 'e) t = fun m f ->
    match m with
    | Ok a -> f a
    | Error e -> raise e 

  let (let*) e f = bind e f
end

(** Configuration type here *)
type arthur =
   (* The top level type has
      - a list of files to lint
      - a rule to apply globally
      - a list of rules for each top level function
    *) 
   | Top of files * global_rule * local_rules
 
and files = | All | Specific of file list
and file = string

and global_rule =
   (* A global rule is just a rule *)
   | GlobalRule of rule

and local_rules = local_rule list
and local_rule =
   (* A local rule includes
      - the top level function name to consider
      - the file (optional) in which to consider it
      - the rule to apply
    *)
  | LocalRule of string * string option * rule
 (* A rule is just a list of checks to enable | a list of checks to disable *)
and rule =
  | Enable of string list
  | Disable of string list


module Y = Yaml
open EitherM

(* Turn a Yaml value into a arthur value *)
let rec parse_arthur (top: Y.value) : (arthur, string) result =
  match top with
  | `O ls ->
     let* files = parse_files ls in
     let* global_rule = parse_global_rule ls in
     let* local_rules = parse_local_rules ls in
     return (Top (files, global_rule, local_rules))
  | _ -> raise "error: ill-formatted arthur file"

and parse_files (yaml: (string * Y.value) list) : (files, string) result =
  let opt = yaml |> List.find_opt (fun (s,_) -> s = "files") in
  match opt with
  | None -> return All
  | Some (_, ls) ->
     let* files = parse_str_list ls in
     if List.length files = 0 then return All
     else
       return (Specific files)
  
and parse_global_rule (yaml: (string * Y.value) list) : (global_rule, string) result = 
  let opt = yaml |> List.find_opt (fun (s,_) -> s = "globals") in
  match opt with
  | None -> (* Disable no rules globally *)
     return (GlobalRule (Disable []))
  | Some (_, glob) ->
     let* rule = parse_rule glob in
     return (GlobalRule rule)

and parse_local_rules (yaml: (string * Y.value) list) : (local_rules, string) result =
  let only_funcs = List.filter (fun (s, _) -> s <> "files" && s <> "globals") yaml in
  let rules = List.map parse_local_rule only_funcs in
  List.fold_left (fun rest m ->
      let* locals = rest in
      let* curr = m in
      return (curr :: locals) ) (return []) rules

and parse_local_rule ((fname,yaml): (string * Y.value)) : (local_rule, string) result =
  begin match yaml with
  | `A ([`String s; `O r]) ->
     let* rule = parse_rule (`O r) in
     return (LocalRule (fname, Some s, rule))
  | `A ([`O r]) ->
     let* rule = parse_rule (`O r) in
     return (LocalRule (fname, None, rule))
  | _ ->
     raise "error: a local rule is an optional filename followed by a single rule (either enable / disable"
  end

and parse_rule (yaml : Y.value) : (rule, string) result =
  begin match yaml with
  | `O [(flag, inner)] ->
    begin match flag with
     | "enable" ->
       let* enables = parse_str_list inner in
       return (Enable enables)
     | "disable" -> 
       let* disables = parse_str_list inner in
       return (Disable disables)
     | _ -> Result.error "error: not a valid tag. Acceptable values are precisely one of enable | disable"
    end
  | _ -> Result.error "error: rule not formatted correctly"
  end
  
and parse_str_list (yaml: Y.value) : (string list, string) result =
 begin match yaml with
 | `A ls -> if List.for_all (fun e -> match e with | `String _ -> true | _ -> false) ls then
              List.map (fun e -> match e with | `String s -> s | _ -> failwith "impossible") ls |>
                return
            else
              raise "error: one of the entries in this list isn't a string"
  | _ -> raise "error: not a string list"
 end

(* Pretty print an arthur configuration file *)
let rec pp_arthur (top: arthur) : string =
  match top with
  | Top (files, global, locals) ->
     let fprint = Printf.sprintf "Files being linted: %s \n" (pp_files files) in
     let gprint = Printf.sprintf "Rules applied to every function:\n %s\n" (pp_global global) in
     let locals = Printf.sprintf "Additional rules applied to specific functions: %s" (pp_locals locals) in
     let str_arthur = Printf.sprintf "%s \n %s \n %s" fprint gprint locals in
     str_arthur

and pp_files (files: files) : string =
  match files with
  | All -> "** Linting all files in the directory **"
  | Specific s -> List.fold_left (fun acc s -> acc ^ "\n - " ^ s) "" s

and pp_global (glob: global_rule) : string =
  begin match glob with
  | GlobalRule r ->
     match r with
     | Enable es ->
        Printf.sprintf "******** Enabled  ******** %s\n"  (pp_rule es)
     | Disable ds ->
        Printf.sprintf "******** Disabled ******** %s\n"  (pp_rule ds)
  end
and pp_locals (locs : local_rules) : string =
  List.fold_left (fun rules r -> rules ^ "\n " ^ (pp_local r)) "" locs

and pp_local (loc : local_rule) : string =
  begin match loc with
  | LocalRule (func, _, r) ->
     match r with
     | Enable es ->
        Printf.sprintf "******** Enabled for %s ******** %s\n" func (pp_rule es)
     | Disable ds ->
        Printf.sprintf "******** Disabled for %s ******** %s\n" func (pp_rule ds)
  end

and pp_rule (es : string list) : string =
  List.fold_left (fun rules r -> rules ^ "\n - " ^ r) "" es 
     

(* Takes a configuration file a list of rules, and returns a refined list *)
let rec run_arthur
      (cfg: arthur)
      (checks: (string * 'a) list)
      (current_file: file)
      (current_func: string)
    : (string * 'a) list = 
  match cfg with
  | Top (files, GlobalRule r, []) ->
     if (run_files files current_file) then
       run_rule_global r checks
     else []
  | Top (files, glob, locals) ->
     if (run_files files current_file) then
          run_locals glob locals checks current_file current_func
     else []

and run_files (f: files) (current_file: string) : bool =
  match f with
  | All -> true
  | Specific fs -> List.exists (fun s -> s = current_file) fs

and run_locals (glob: global_rule)
               (loc: local_rules)
               (checks: (string * 'a) list)
               (current_file: string)
               (current_func: string) : (string * 'a) list =
  loc |>
    List.fold_left
      (fun acc x ->
        let checks = run_local glob x checks current_file current_func in
        acc @ (checks)) [] |>
    
    List.sort_uniq (fun (l,_) (r,_) -> String.compare l r)
 
and run_local
  (glob: global_rule)
  (loc: local_rule)
  (checks: (string * 'a) list)
  (current_file: string)
  (current_func: string) : (string * 'a) list =
  (* Turns into a list of rules to run *)
  let set_compute u global local : string list =
    let diff a b : string list = List.filter (fun s ->
                       not @@  List.exists (fun s' -> s = s') b
                     ) a |>
                     List.sort_uniq (String.compare)
                     in
    let union a b : string list  = List.sort_uniq (String.compare) (a @ b) in
    let u = List.map (fst) u in
    match global, local with
    | Enable gr, Enable locr -> union gr locr
    | Enable gr, Disable locr -> diff gr locr
    | Disable gr, Enable locr -> union (diff u gr) locr
    | Disable gr, Disable locr -> diff u (union gr locr)
  in
 begin match glob, loc with
 | GlobalRule r, LocalRule (toplevel_func, Some f, rule) ->
    if (toplevel_func = current_func && f = current_file) then
      let rules = set_compute checks r rule in
      List.filter (fun (a,_) -> List.exists (fun r -> r = a) rules) checks
   else run_rule_global r checks
 | GlobalRule r, LocalRule (toplevel_func, None, rule) ->
    if (toplevel_func = current_func) then
      let rules = set_compute checks r rule in
      List.filter (fun (a,_) -> List.exists (fun r -> r = a) rules) checks
   else run_rule_global r checks
       
 end

and run_rule_global
  (r: rule)
  (checks: (string * 'a) list) : (string * 'a) list =
  let cmp_check (a, _b) (c,_d) : int =
    String.compare a c in
  match r with
  | Enable es ->
     (* Enable is easy - only apply the checks in es - any values in checks that are not in es are ignored *)
     List.filter (fun (s, _) -> List.exists (fun s' -> s' = s) es) checks |>
       (* We'll remove duplicate checks as well :) (although this should never happen) *)
       List.sort_uniq cmp_check

  | Disable ds ->
     (* Disable should take the rules in checks and remove the checks in ds *)
     checks |>
       List.filter (fun (s, _) -> not @@ List.exists (fun s' -> s' = s) ds) |>
       List.sort_uniq cmp_check
     


(** Module implementations *)
type config = arthur

let parse_file_config filename =
  let str = read_file_str filename in 
  match Yaml.of_string str with
  | Ok yaml ->
     let* config = parse_arthur yaml in
     return config
  | Error (`Msg m) -> raise m

let parse_string_config str =
  match Yaml.of_string str with
  | Ok yaml ->
     let* config = parse_arthur yaml in
     return config
  | Error (`Msg m) -> raise m

let pp_config (config: config) : string =
  pp_arthur config

let print_config (config: config) : unit =
  config |> pp_config |> print_string

let print_config_plan (config: config) : unit =
  print_config config

let eval_config config filename funcname checks =
  run_arthur config checks filename funcname 

let default : config =
  Top (All, GlobalRule (Disable []), []) 

(* Serialize an arthur config back into a yaml value *)
let rec config_to_yaml : config -> Y.value = fun config ->
  match config with
  | Top (files, glob, locs) ->
     `O ([files_to_yaml files; global_to_yaml glob] @ locals_to_yaml locs)
     
and files_to_yaml (files: files) : string * Y.value =
  match files with
  | All -> ("files", `A [])
  | Specific fs -> ("files", `A (List.map (fun e -> `String e) fs))

and global_to_yaml (global: global_rule) : string * Y.value =
  match global with
  | GlobalRule r -> ("globals", rule_to_yaml r)

and rule_to_yaml (rule: rule) : Y.value =
  match rule with
  | Enable es ->
     `O [("enable", `A (List.map (fun e -> `String e) es)) ]
  | Disable es ->
     `O [("disable", `A (List.map (fun e -> `String e) es)) ]

and locals_to_yaml (loc: local_rules) : (string * Y.value) list =
  List.map local_rule_to_yaml loc

and local_rule_to_yaml (rule: local_rule) : string * Y.value =
  match rule with
  | LocalRule (toplevel, Some f, r) ->
     (toplevel, `A [`String f; rule_to_yaml r ])
  | LocalRule (toplevel, None, r) ->
     (toplevel, `A [ rule_to_yaml r ])



(* Utility for reading files *)
let sanitize_dir d =
  if d.[String.length d - 1] = '/' then d 
  else d ^ "/"


let files_in_dir dirname = 
  let open Sys in
  let dir = sanitize_dir dirname in
  let fail msg = prerr_endline msg; exit 1 in
  if not (file_exists dir && is_directory dir) 
  then  fail @@ dir ^ " doesn't exist or isn't a directory!"; 
  readdir dir |> Array.to_list |> List.map (fun file -> dir ^ file)

let rec files_in_dir_rec dirname = 
  let open Sys in
  let dir = sanitize_dir dirname in
  if not (file_exists dir && is_directory dir) then []
  else 
    let children = files_in_dir dirname in
    children @ List.concat_map files_in_dir_rec children

let current_config : config ref = ref default
let get_config : unit -> config = fun _ -> !current_config
let set_config : config -> unit = fun c -> current_config := c

let files_to_lint : bool -> string -> string list = fun recurse dirname ->
  match get_config () with
  | Top (All, _, _) -> (if recurse then files_in_dir_rec else files_in_dir) dirname
  | Top (Specific fs, _, _) -> fs

let write_config ?file:(arg1="arthur.yaml") (config: config) : unit =
  let yaml = config_to_yaml config in
  let encoding : Y.encoding = `Utf8 in
  let layout : Y.layout_style = `Block in
  let sc_style : Y.scalar_style = `Any in
  let y_str = Y.to_string
                ~encoding:encoding
                ~scalar_style:sc_style
                ~layout_style:layout yaml in
  match y_str with
  | Ok st -> write_str_file st arg1
  | Error (`Msg m) -> failwith m

let check_config : unit = ()


