open Parsetree
open Report
(* ---------- Extract information from expressions --------------------------------------- *)

let desc_of_expr (desc: Parsetree.expression) : Parsetree.expression_desc =
  let {pexp_desc; pexp_loc;pexp_loc_stack;pexp_attributes} = desc in pexp_desc
    
let loc_of_expr (desc: Parsetree.expression) : warn_loc =
  let {pexp_desc; pexp_loc;pexp_loc_stack;pexp_attributes} = desc in
  let warn_loc_of_loc (l: Location.t) : warn_loc =
    let start = l.loc_start in
    let fin = l.loc_end in
    Report.warn_loc start.pos_lnum
      fin.pos_lnum
      (start.pos_cnum - start.pos_bol)
      (fin.pos_cnum - fin.pos_bol) in
  
  warn_loc_of_loc pexp_loc
    
let locstack_of_expr (desc: Parsetree.expression) : Location.t list =
  let {pexp_desc; pexp_loc;pexp_loc_stack;pexp_attributes} = desc in pexp_loc_stack

let attrs_of_expr (desc: Parsetree.expression) : Parsetree.attributes =
  let {pexp_desc; pexp_loc;pexp_loc_stack;pexp_attributes} = desc in pexp_attributes





(* ---------- Types from attributes ------------------------------------------------------- *)


let string_of_payload : Parsetree.payload -> string = function
  | PStr s -> "Pstr ()"
  | PSig s -> "PSig ()"
  | PTyp s -> "PTyp ()"
  | PPat _ -> "PPat ()"

let string_of_attr (att: Parsetree.attribute) : string =
  att.attr_name.txt ^ "\n" ^ string_of_payload att.attr_payload

let print_attributes (att: Parsetree.attributes) : unit =
  List.iter (fun i -> print_endline @@ string_of_attr i) att

let type_of_expr (desc: Parsetree.expression) : unit =
 ()


(* Handle recursion down *)
