(** 
   Parser for the json type to an internal type 
*)

(* Retry *)


(* Useful combinators *)

module OptInst = struct
  let return : 'a -> 'a option = fun e -> Some e

  let bind : 'a option -> ('a -> 'b option) -> 'b option = fun e f ->
    match e with
    | None -> None
    | Some i -> f i

  let (>>=) e f = bind e f

  let pure : 'a -> 'a option = return

  let zap : ('a -> 'b) option -> 'a option -> 'b option = fun fo a ->
    begin match fo with
    | None -> None
    | Some f ->
      begin match a with
      | None -> None
      | Some i -> Some (f i)
      end
    end

  let (<*>) f s = zap f s
      
  let fmap : ('a -> 'b) -> 'a option -> 'b option = fun f a ->
    a >>= (fun e -> pure (f e) )
    
  let ( <$> ) f a = fmap f a
  (* Functor pipe *)
  let (|>>) s f = f <$> s

  let (|*>) s f = pure f <*> s
    
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
  | Arthur of global * func list
and global =
  | Global of flag
and func =
  | Func of letname * flag
and flag =
  | Disable of files


let rec pp_arthur : arthur -> string = fun (Arthur (glob, funcs)) ->
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
  Arthur (Global (Disable []), [])


(** Looks for an arthur.json file *)
let from_file : string -> Yojson.Basic.t option = fun s ->
  try Some (Yojson.Basic.from_file s)
  with _ -> None

(* Recursive descent parser for json to arthur *)
let rec json_to_arthur : Yojson.Basic.t option -> arthur = function
  | None -> 
    default
  | Some json ->
    let glob = json_to_global json in
    let locals = json_to_funcs json in 
    Arthur (glob, locals)
      
and json_to_global : Yojson.Basic.t -> global = fun j ->
  match PUtils.project "global" j with
  | None ->
    (* Default action: Globally, disable nothing *)
    Global (Disable [])
  | Some o -> Global (json_to_flag o)

and json_to_funcs : Yojson.Basic.t -> func list = fun j ->
  let open OptInst in
  let l = PUtils.project "locals" j >>=
    (fun o ->
       PUtils.list o |*>
       List.map (json_to_func) |*>
       List.filter_map (fun e -> e)) in
  match l with
  | None -> []
  | Some i -> i

and json_to_func : Yojson.Basic.t -> func option = fun j ->
  let open OptInst in
  let func_name = PUtils.key_func j in
  let flag = func_name
    >>= (fun s -> PUtils.project s j)
            |>> json_to_flag in
  match func_name, flag with
  | Some n, Some f -> Some (Func (n, f))
  | _ -> None
    
and json_to_flag : Yojson.Basic.t -> flag = fun j ->
  let l = PUtils.project "disable" j in
  let ls = match l with
    | None -> []
    | Some v -> match PUtils.list v with | None -> [] | Some e -> e in
  let ls' = PUtils.string_list ls in
  Disable ls'
    
