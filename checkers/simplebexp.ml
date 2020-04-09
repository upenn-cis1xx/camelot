open Asttypes
open Parsetree
open Longident
open Check
open Report


(* ------------------ Checks rule: if cond then true else false ------------------------- *)
module IfReturnLit : CHECK = struct
  let check ({location;code} : Report.lctxt) : warn option =
    begin match code with
    | EIfThenElse (test, b_then, b_else) ->
      begin match b_then, b_else with
        | Pexp_construct ({txt = Lident "true"}, _),
          Pexp_construct ({txt = Lident "false"}, _) ->
          Some ({
              loc = location;
              violation = (BPat (IfReturnsLit));
            })
        | _ -> None
        
      end
    | _ -> None
    end
end

(* ------------------ Checks rule: if cond then false else true ------------------------- *)
module IfReturnLitInv : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    begin match code with
    | EIfThenElse (test, b_then, b_else) ->
      begin match b_then, b_else with
        | Pexp_construct ({txt = Lident "false"}, _),
          Pexp_construct ({txt = Lident "true"}, _) ->
          Some ({
              loc = (location);
              violation = (BPat (IfReturnsLitInv));
            })
        | _ -> None
        
      end
    | _ -> None
    end
end





(* ------------------ Checks rule: if cond then cond else _ ----------------------------- *)
module IfReturnsCond : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | EIfThenElse (test, b_then, _) ->
        begin match test, b_then with
          | Pexp_ident ({txt = Lident x; _}),
            Pexp_ident ({txt = Lident y; _}) ->
            if x = y then
              Some ({
                  loc = (location);
                  violation = (BPat (IfReturnsCond))
                })
            else None
          | _ -> None
        end
      | _ -> None
    end
end



(* ------------------ Checks rule: if not cond then x else y ---------------------------- *)
module IfCondNeg : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    let open Utils in
    begin match code with
      | EIfThenElse (test, _, _) ->
        begin match test with
          | Pexp_apply ( fcall , _ ) ->
            let fcall = Utils.desc_of_expr fcall in
            begin match fcall with
              | Pexp_ident ({txt = Lident "not"}) ->
                Some
                  ({
                    loc = (location);
                    violation = (BPat (IfCondNeg))
                  })
              | _ -> None
            end
          | _ -> None
        end
      |_ -> None
    end
end


(* ------------------ Checks rule: if x then true else y  ------------------------------ *)
module IfReturnsTrue : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    let open Utils in
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
          begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "true"}, _),
            Pexp_ident ({txt = Lident y})
            -> Some ({
              loc = (location);
              violation = (BPat (IfReturnsTrue))
            })
          | _ -> None
        end
      | _ -> None
    end
end


(* ------------------ Checks rule: if x then y else false ------------------------------ *)
module IfFailFalse : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_else, b_then with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_ident ({txt = Lident y})
            -> Some ({
              loc = (location);
              violation = (BPat (IfFailFalse))
            })
          | _ -> None
        end
      | _ -> None
    end
end


(* ------------------ Checks rule: if x then false else y  ----------------------------- *)
module IfSuccFalse : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_ident ({txt = Lident y})
            -> Some ({
                loc = (location);
                violation = (BPat (IfSuccFalse))
              })
          | _ -> None
        end
      | _ -> None
  end
end

(* ------------------ Checks rule: if x then y else true   ----------------------------- *)
module IfFailTrue : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_ident ({txt = Lident y}),
            Pexp_construct ({txt = Lident "true"}, _)
            -> Some ({
                loc = (location);
                violation = (BPat (IfFailTrue))
              })
          | _ -> None
        end
      | _ -> None
    end
end

let checks  = [ IfReturnLit.check
              ; IfReturnLitInv.check
              ; IfReturnsCond.check
              ; IfCondNeg.check
              ; IfReturnsTrue.check
              ; IfFailFalse.check
              ; IfSuccFalse.check
              ; IfFailTrue.check 
              ]


