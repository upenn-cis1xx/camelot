open Warnloc

type patternctxt = {
  location: warn_loc;
  source: string;
  pattern: Parsetree.expression_desc
}


let mk_pc location source pattern = {location; source; pattern}
