open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Report
open List


let allchecks = Simplebexp.checks 
          
let allexps = ref []


module MapAst = struct
  let ifthenelse m test b_then b_else = Pexp_ifthenelse (m test, m b_then, Some (m b_else) )
  let matchcases m e cs = Pexp_match (m e, cs) (* TODO, map expression mapper over each case *)
  let bopapply m op (l, el) (r, er) = Pexp_apply (m op, [(l, m el); (r, m er)])
end

let rec linter_mapper =
  { 
    default_mapper with
    expr = expr_mapper;
  }

and 
expr_mapper (mapper: Ast_mapper.mapper) (expr: Parsetree.expression) : Parsetree.expression =
  let open Utils in
  let desc = desc_of_expr expr in
  let loc : Report.warn_loc = loc_of_expr expr in
  let emap = linter_mapper.expr mapper in
  begin match desc with
    
  | Pexp_ifthenelse (test, bthen, Some belse) ->
    let (d_test, d_then, d_else) = (desc_of_expr test, desc_of_expr bthen, desc_of_expr belse) in
    let e_lint = EIfThenElse (d_test, d_then, d_else) in
    allexps := {location=loc; code=e_lint} :: !allexps;
    {expr with pexp_desc=MapAst.ifthenelse emap test bthen belse}

  | Pexp_match (e, cs) ->
    let e_lint = PPatternMatch (e.pexp_desc, cs) in
    allexps := {location=loc;code = e_lint} :: !allexps;
    {expr with pexp_desc=MapAst.matchcases emap e cs}

  (* Look for binary operands *)
  | Pexp_apply (e, [i1; i2]) ->
    let is_bop (op: string) (e : Parsetree.expression_desc) : bool =
      match e with
      | Pexp_ident ({txt = Lident x}) -> x = op
      | _ -> false
    in
    (* Unit action *)
    (if is_bop "=" e.pexp_desc then
      let e_lint = EqApply ((snd i1).pexp_desc, (snd i2).pexp_desc) in
      allexps := {location=loc; code=e_lint} :: !allexps);

    {expr with pexp_desc=MapAst.bopapply emap e i1 i2 } 
    
  | _ -> default_mapper.expr mapper expr
  end        



let lint : unit -> unit = fun _ ->
  List.iter (fun e ->
      let e_linted = allchecks |>
                     List.map (fun fc -> fc e) |>
                     List.filter (fun i -> match i with | None -> false | Some _ -> true) |>
                     List.map (fun i -> match i with | Some e -> e | None -> failwith "frick") in
                       
      List.iter (fun hint -> print_endline @@  string_of_hint hint ) e_linted
                       
    ) (List.rev (!allexps))



let lmap = fun argv ->
  linter_mapper
  
let () =
  register "linter" lmap;
  print_endline @@ "Linting: " ^ !Location.input_name;
  lint ()
