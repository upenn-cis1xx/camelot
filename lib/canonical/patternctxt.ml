open Warnloc

type _ pctxt =
  | E : { location: warn_loc;
          source: string;
          pattern: Parsetree.expression_desc
        } -> Parsetree.expression_desc pctxt

  | P : {location: warn_loc;
         source: string;
         pattern: Parsetree.structure_item_desc
        } -> Parsetree.structure_item_desc pctxt

let ctxt_of_expr filename (expr: Parsetree.expression) : Parsetree.expression_desc pctxt =
  let loc = Warnloc.warn_loc_of_loc filename expr.pexp_loc in
  let raw = Pprintast.string_of_expression expr in
  E {location = loc; source = raw; pattern = expr.pexp_desc}

let ctxt_of_structure source (structure: Parsetree.structure_item) : Parsetree.structure_item_desc pctxt =
  let loc = Warnloc.warn_loc_of_loc source structure.pstr_loc in
  P {location = loc; source; pattern = structure.pstr_desc}

