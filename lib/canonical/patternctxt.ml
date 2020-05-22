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

let ctxt_of_expr source (expr: Parsetree.expression) : Parsetree.expression_desc pctxt =
  let loc = Warnloc.warn_loc_of_loc source expr.pexp_loc in
  E {location = loc; source; pattern = expr.pexp_desc}

let ctxt_of_structure source (structure: Parsetree.structure_item) : Parsetree.structure_item_desc pctxt =
  let loc = Warnloc.warn_loc_of_loc source structure.pstr_loc in
  P {location = loc; source; pattern = structure.pstr_desc}

