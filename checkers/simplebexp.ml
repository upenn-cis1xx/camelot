open Asttypes
open Parsetree
open Longident
open Check
open Report


(* ------------------ Checks rule: if cond then true else false ------------------------- *)
module IfReturnLit : CHECK = struct
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

(* ------------------ Checks rule: if cond then false else true ------------------------- *)
module IfReturnLitInv : CHECK = struct
  let check (exp : expr) : warn option =
    begin match exp with
    | EIfThenElse (test, b_then, b_else) ->
      begin match b_then, b_else with
        | Pexp_construct ({txt = Lident "false"}, _),
          Pexp_construct ({txt = Lident "true"}, _) ->
          Some ({
              loc = (warn_loc 1 1 1);
              violation = (BPat (IfReturnsLitInv));
            })
        | _ -> None
        
      end
    | _ -> None
    end
end


let checks  = [ IfReturnLit.check
              ; IfReturnLitInv.check
              ]

