open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Report
open List

let allchecks = Simplebexp.checks
let allexps = ref []


let rec linter_mapper =
  { 
    default_mapper with
    expr = expr_mapper;
  }

and 
expr_mapper (mapper: Ast_mapper.mapper) (expr: Parsetree.expression) : Parsetree.expression =
  let open Utils in
  let desc = desc_of_expr expr in
  begin match desc with
  | Pexp_ifthenelse (test, bthen, Some belse) ->
    let (d_test, d_then, d_else) = (desc_of_expr test, desc_of_expr bthen, desc_of_expr belse) in
    let e_lint = EIfThenElse (d_test, d_then, d_else) in
    allexps := e_lint :: !allexps;
    default_mapper.expr mapper expr
  | _ -> default_mapper.expr mapper expr
  end        



let lint : unit -> unit = fun _ ->
  List.iter (fun e ->
      let e_linted = allchecks |>
                     List.map (fun fc -> fc e) |>
                     List.filter (fun i -> match i with | None -> false | Some _ -> true) |>
                     List.map (fun i -> match i with | Some e -> e | None -> failwith "frick") in
                       
      List.iter (fun warn -> print_endline @@ "Warning: " ^ string_of_warn warn ) e_linted
                       
    ) !allexps



let rec lmap = fun argv -> linter_mapper
  
let () =
  register "linter" lmap;
  print_endline "Linting: ";
  lint ()
