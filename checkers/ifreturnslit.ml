open Asttypes
open Parsetree
open Longident
open Check
open Report

module IfReturnsLit : CHECK = struct

  let check (exp : expr) : warn option =
    begin match exp with
    | EIfThenElse (test, b_then, b_else) ->
      begin match b_then, b_else with
        | Pexp_construct ({txt = Lident "true"}, _),
          Pexp_construct ({txt = Lident "false"}, _) ->
          Some ({
              loc = (warn_loc 1 1 1);
              violation = (BPat (IfReturnsLit));
            })
        | _ -> None
        
      end
    | _ -> None
    end
      
      
  
end
