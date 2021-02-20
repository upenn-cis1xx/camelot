open Warnloc

type _ pctxt =
  (* Expression constructor *)
  | E : { location: warn_loc;
          source: string;
          pattern: Parsetree.expression_desc
        } -> Parsetree.expression_desc pctxt
  (*  Top Level let constructor *)
  | P : {location: warn_loc;
         source: string;
         pattern: Parsetree.structure_item_desc
        } -> Parsetree.structure_item_desc pctxt
  (* File Linting *)
  | L : { source: string;
          pattern: file
        } -> file pctxt


(*
   Using an explicit definition wrapping for the abstract
   type for in_channel.

   For the reason why we have to do this, see
github.com/ocaml/ocaml/issues/7360.
   Essentially, if we wanted to make just a
in_channel pctxt, the OCaml typechecker would have no idea how
in_channel was defined. In theory, it could have been that
type in_channel = Parsetree.expression_desc, which means that
it is possible for more than one constructor to have produced
a Parsetree.x ctxt. Therefore the exhaustiveness checker would trigger,
forcing all cases to have been matched, since we don't have the invariant of
one constructor per ast type.

*)
and file = F of in_channel

let ctxt_of_expr filename (expr: Parsetree.expression) : Parsetree.expression_desc pctxt =
  let loc = Warnloc.warn_loc_of_loc filename expr.pexp_loc in
  let raw = Pprintast.string_of_expression expr in
  E {location = loc; source = raw; pattern = expr.pexp_desc}

let ctxt_of_structure source (structure: Parsetree.structure_item) : Parsetree.structure_item_desc pctxt =
  let loc = Warnloc.warn_loc_of_loc source structure.pstr_loc in
  let raw = Pprintast.string_of_structure [structure] in
  P {location = loc; source = raw; pattern = structure.pstr_desc}

let ctxt_for_lexical source channel : file pctxt =
  L {source; pattern = F channel}
