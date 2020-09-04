(** 
   Parser module for the json object to an internal representation 
*)


(* Useful combinators *)

(** Optional Instance: Provide combinators for monadic let bindings *)
module OptInst = struct

  let return : 'a -> 'a option = fun e -> Some e

  let bind : 'a option -> ('a -> 'b option) -> 'b option = fun e f ->
    match e with
    | None -> None
    | Some i -> f i

  let (let*) e f = bind e f

  let with_default : 'a option -> 'a option -> 'a option = fun current replace ->
    match current with
    | None -> replace
    | Some _ -> current

  let prod : 'a option -> 'b option -> ('a * 'b) option = fun l r ->
    match l, r with
    | Some x, Some y -> Some (x,y)
    | _ -> None


  let (and+) l r = prod l r

  (** x <?> y : read as optional choice combinator. If x exists choose it. If not choose y.
      Intended as a choose default type of thing.
      This is exactly haskells alternative. But for parsing, <?> looks better - signifies we're
      working with optionals.
  *)
  let ( <?> ) a b = with_default a b
end

module Default = struct
end

(** Utility for working with YoJson *)
module PUtils = struct
  let fail : (unit -> 'a) -> 'a option = fun f ->
    try Some (f ()) with _ -> None

  let project : string -> Yojson.Basic.t -> Yojson.Basic.t option = fun s j ->
    fail (fun _ -> Yojson.Basic.Util.member s j )

  let list : Yojson.Basic.t -> Yojson.Basic.t list option = fun e ->
    fail (fun _ -> Yojson.Basic.Util.to_list e )

  let string : Yojson.Basic.t -> string option = Yojson.Basic.Util.to_string_option

  let string_list : Yojson.Basic.t list -> string list = fun l -> 
    List.filter_map (string) l


  let key_func : Yojson.Basic.t -> string option = fun o ->
    match Yojson.Basic.Util.keys o with
    | [s] -> Some s
    | _ -> None

end

(** The internal arthur type - 
    Arthur's configuration is:
    - a list of files to lint
    - a global configuration, of rules to apply
    - a local configuration of functions that have rules to disable
*)
type arthur =
  | Arthur of files * global * func list

(** A global is just a flag *)
and global =
  | Global of flag

(** A func is just a top-level function name, and a flag to apply *)
and func =
  | Func of letname * flag

(** A flag is just a list of files to disable *)
and flag =
  | Disable of files
and files = string list
and letname = string


(** A function for pretty_printing an arthur configuration *)
let rec pp_arthur : arthur -> string = fun (Arthur (_files, glob, funcs)) ->
  "Arthur (\n" ^
  pp_global glob ^ "\n," ^
  pp_funcs funcs ^ "\n" ^ 
  ")\n"

and pp_global : global -> string = fun (Global f) ->
  "Global (\n" ^
  pp_flag f ^ "\n" ^
  ")\n"

and pp_funcs : func list -> string = fun l ->
  "[\n" ^
  (List.map (pp_func) l |>
   String.concat "\n") ^
  "\n]\n"

and pp_func : func -> string = fun (Func (name, flag)) ->
  "Func( " ^ name ^ "\n" ^
  pp_flag flag
and pp_flag : flag -> string = fun (Disable fs) ->
  "disable: " ^ "[" ^ String.concat ", " fs ^ "]"

let default : arthur =
  Arthur ([], Global (Disable []), [])


(** Looks for an arthur.json file *)
let from_file : string -> Yojson.Basic.t option = fun s ->
  try Some (Yojson.Basic.from_file s)
  with _ -> None

(** Recursive descent parser for json to arthur *)
let rec json_to_arthur : Yojson.Basic.t option -> arthur = fun tl ->
  let open OptInst in
  let parse_in =
    let* raw = tl in 
    let* glob = json_to_global raw
    and+ locals = json_to_funcs raw
    and+ toLint = files raw in
    return (Arthur (toLint, glob, locals)) <?> return default
  in
  match parse_in with
  | None -> default
  | Some v -> v

and files : Yojson.Basic.t -> files option = fun j ->
  let open OptInst in
  let* toLint = PUtils.project "toLint" j in
  let* lint_list = PUtils.list toLint <?> (return []) in
  return @@ PUtils.string_list lint_list

and json_to_global : Yojson.Basic.t -> global option = fun j ->
  let open OptInst in
  let* globs = PUtils.project "global" j in
  let* gls = json_to_flag globs <?> return @@ (Disable []) in
  return @@ Global gls

and json_to_funcs : Yojson.Basic.t -> func list option= fun j ->
  let open OptInst in
  let* locals = PUtils.project "locals" j in
  let* listlocals = PUtils.list locals in
  let lv = listlocals |> List.filter_map (json_to_func) in
  return lv

and json_to_func : Yojson.Basic.t -> func option = fun j ->
  let open OptInst in
  let* func_name = PUtils.key_func j in
  let* proj_field = PUtils.project func_name j in
  let* flag = json_to_flag proj_field in
  return (Func(func_name, flag))

and json_to_flag : Yojson.Basic.t -> flag option  = fun j ->
  let open OptInst in
  let* l = PUtils.project "disable" j in
  let* ls = PUtils.list l <?> return [] in
  let ls' = PUtils.string_list ls in
  return @@ Disable ls'

