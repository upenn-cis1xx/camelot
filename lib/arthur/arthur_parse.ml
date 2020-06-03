(** 
   Parser for the json type to an internal type 
*)

(* Retry *)


(* Useful combinators *)

module OptInst = struct

  let return : 'a -> 'a option = fun e -> Some e

  let pure : 'a -> 'a option = return

  let bind : 'a option -> ('a -> 'b option) -> 'b option = fun e f ->
    match e with
    | None -> None
    | Some i -> f i

  let (>>=) e f = bind e f

  let (let*) e f = bind e f

  let zap : ('a -> 'b) option -> 'a option -> 'b option = fun fo a ->
    begin match fo, a with
      | Some f, Some x -> Some (f x)
      | _ -> None
    end

  let (<*>) f s = zap f s
      
  let map : ('a -> 'b) -> 'a option -> 'b option = fun f a ->
    match a with
    | None -> None
    | Some x -> Some (f x)
    
  let ( <$> ) f a = map f a

  let (let+) s f = map f s

  let product : 'a option -> 'b option -> ('a * 'b) option = fun a b ->
    match a, b with
    | Some x, Some y -> Some (x,y)
    | _ -> None

  let (and+) a b = product a b
  let (and*) a b = product a b 
  
  (* Functor pipe *)
  let (|>>) s f = f <$> s

  let (|*>) s f = pure f <*> s
    
end

module Default = struct
  type 'a default =
    | Found of 'a
    | Default of 'a

  let return : 'a -> 'a default = fun x -> Found x

  let pure : 'a -> 'a default = return

  let bind : 'a default -> ('a -> 'b default) -> 'b default = fun e f ->
    match e with
    | Found e -> f e
    | Default e -> f e

  let (let*) e f = bind e f

  let map : ('a -> 'b) -> 'a default -> 'b default = fun f -> function
    | Found e -> Found (f e)
    | Default e -> Default (f e)

  let (let+) s f = map f s

                     
end


(* Utility for working with YoJson o god so much fucking utility *)
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


type file = string
type files = file list
type letname = string

type arthur =
  | Arthur of files * global * func list
and global =
  | Global of flag
and func =
  | Func of letname * flag
and flag =
  | Disable of files


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

(* Recursive descent parser for json to arthur *)
let rec json_to_arthur : Yojson.Basic.t option -> arthur = fun tl ->
  let open OptInst in
  let parse_in = match tl with
  | None -> 
    return default
  | Some json ->
    let* glob = json_to_global json in
    let* locals = json_to_funcs json in
    let* toLint = files json in
    return @@ Arthur (toLint , glob, locals)
  in
  match parse_in with
  | None -> default
  | Some v -> v

and files : Yojson.Basic.t -> files option = fun j ->
  let open OptInst in
  let* toLint = PUtils.project "toLint" j in
  match PUtils.list toLint with
  | None -> return []
  | Some v -> return @@ PUtils.string_list v
      
and json_to_global : Yojson.Basic.t -> global option = fun j ->
  let open OptInst in
  let* globs = PUtils.project "global" j in
  let gls = json_to_flag globs in
  match gls with
  | None -> return @@ Global (Disable [])
  | Some e -> return @@ Global e

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
  let* ls = match PUtils.list l with
    | None -> Some []
    | Some e -> Some e in

  let ls' = PUtils.string_list ls in
  return (Disable ls')
    
