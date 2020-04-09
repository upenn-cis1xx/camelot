open Asttypes
open Parsetree
open Longident
open Check
open Report


(* ------------------ Checks rule: if cond then true else false ------------------------- *)
module IfReturnLit : CHECK = struct

  let pattern = "if cond then true else false"
    
  let fix = "replacing with cond"
    
  let check ({location;code} : Report.lctxt) : hint option =
    begin match code with
    | EIfThenElse (test, b_then, b_else) ->
      begin match b_then, b_else with
        | Pexp_construct ({txt = Lident "true"}, _),
          Pexp_construct ({txt = Lident "false"}, _) ->
          mk_hint location (BPat IfReturnsLit) pattern fix
        | _ -> None
        
      end
    | _ -> None
    end

end

(* ------------------ Checks rule: if cond then false else true ------------------------- *)
module IfReturnLitInv : CHECK = struct

  let pattern = "if cond then false else true"

  let fix = "replacing with not cond"
    
  let check ({location;code} : lctxt) : hint option =
    begin match code with
    | EIfThenElse (test, b_then, b_else) ->
      begin match b_then, b_else with
        | Pexp_construct ({txt = Lident "false"}, _),
          Pexp_construct ({txt = Lident "true"}, _) ->
          mk_hint location (BPat IfReturnsLitInv) pattern fix
        | _ -> None
        
      end
    | _ -> None
    end
end





(* ------------------ Checks rule: if cond then cond else _ ----------------------------- *)
module IfReturnsCond : CHECK = struct

  let pattern = "if cond then cond else _"

  let fix = "replacing with cond || _"
    
  let check ({location;code} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (test, b_then, _) ->
        begin match test, b_then with
          | Pexp_ident ({txt = Lident x; _}),
            Pexp_ident ({txt = Lident y; _}) ->
            if x = y then
              mk_hint location (BPat IfReturnsCond) pattern fix
            else None
          | _ -> None
        end
      | _ -> None
    end
end



(* ------------------ Checks rule: if not cond then x else y ---------------------------- *)
module IfCondNeg : CHECK = struct

  let pattern = "if not cond then x else y"

  let fix = "swapping the then and else branches"
    
  let check ({location;code} : lctxt) : hint option =
    let open Utils in
    begin match code with
      | EIfThenElse (test, _, _) ->
        begin match test with
          | Pexp_apply ( fcall , _ ) ->
            let fcall = Utils.desc_of_expr fcall in
            begin match fcall with
              | Pexp_ident ({txt = Lident "not"}) ->
                mk_hint location (BPat IfCondNeg) pattern fix
              | _ -> None
            end
          | _ -> None
        end
      |_ -> None
    end
end


(* ------------------ Checks rule: if x then true else y  ------------------------------ *)
module IfReturnsTrue : CHECK = struct

  let pattern = "if x then true else y"

  let fix = "rewriting using a boolean operator like && or ||"
    
  let check ({location;code} : lctxt) : hint option =
    let open Utils in
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
          begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "true"}, _),
            Pexp_ident ({txt = Lident y}) ->
            mk_hint location (BPat IfReturnsTrue) pattern fix
          | _ -> None
        end
      | _ -> None
    end
end


(* ------------------ Checks rule: if x then y else false ------------------------------ *)
module IfFailFalse : CHECK = struct

  let pattern = "if x then y else false"

  let fix = "rewriting using a boolean operator like && or ||"
    
  let check ({location;code} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_else, b_then with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_ident ({txt = Lident y}) ->
            mk_hint location (BPat IfFailFalse) pattern fix
          | _ -> None
        end
      | _ -> None
    end
end


(* ------------------ Checks rule: if x then false else y  ----------------------------- *)
module IfSuccFalse : CHECK = struct

  let pattern = "if x then false else y"

  let fix = "rewriting using a boolean operator like && or ||"

  let check ({location;code} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_ident ({txt = Lident y}) ->
            mk_hint location (BPat IfSuccFalse) pattern fix
          | _ -> None
        end
      | _ -> None
  end
end

(* ------------------ Checks rule: if x then y else true   ----------------------------- *)
module IfFailTrue : CHECK = struct

  let pattern = "if x then y else true"

  let fix = "rewriting using a boolean operator like && or ||"

  let check ({location;code} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_ident ({txt = Lident y}),
            Pexp_construct ({txt = Lident "true"}, _) ->
            mk_hint location (BPat IfFailTrue) pattern fix
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


