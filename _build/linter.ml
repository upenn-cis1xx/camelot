open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Report

let rec linter_mapper argv =
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
    let f = Ifreturnslit.IfReturnsLit.check in
    let ret = f e_lint in
    begin match ret with
      | None -> print_endline "No suggestions"
      | Some i -> print_endline "found suggestion"
    end;
    default_mapper.expr mapper expr
  | _ -> default_mapper.expr mapper expr
  end        


    



let () = register "linter" linter_mapper
