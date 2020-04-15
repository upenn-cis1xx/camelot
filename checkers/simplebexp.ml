open Asttypes
open Parsetree
open Longident
open Check
open Report


(* ------------------ Checks rule: if cond then true else false ------------------------- *)
module IfReturnLit : CHECK = struct

  let fix = "replacing the if statement with just the condition"

  let check ({location;code; src} : Report.lctxt) : hint option =
    begin match code with
      | EIfThenElse (test, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "true"}, _),
            Pexp_construct ({txt = Lident "false"}, _) ->
            mk_hint location (BPat IfReturnsLit) src fix
          | _ -> None

        end
      | _ -> None
    end

end

(* ------------------ Checks rule: if cond then false else true ------------------------- *)
module IfReturnLitInv : CHECK = struct

  let fix = "replacing the if statement with (not condition)"

  let check ({location;code; src} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (test, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_construct ({txt = Lident "true"}, _) ->
            mk_hint location (BPat IfReturnsLitInv) src fix
          | _ -> None

        end
      | _ -> None
    end
end





(* ------------------ Checks rule: if cond then cond else _ ----------------------------- *)
module IfReturnsCond : CHECK = struct

  let fix = "replacing the if statement with the condition || fail_branch "

  let check ({location; code; src} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (test, b_then, _) ->
        begin match test, b_then with
          | Pexp_ident ({txt = Lident x; _}),
            Pexp_ident ({txt = Lident y; _}) ->
            if x = y then
              mk_hint location (BPat IfReturnsCond) src fix
            else None
          | _ -> None
        end
      | _ -> None
    end
end



(* ------------------ Checks rule: if not cond then x else y ---------------------------- *)
module IfCondNeg : CHECK = struct

  let fix = "swapping the then and else branches"

  let check ({location; code; src} : lctxt) : hint option =
    let open Utils in
    begin match code with
      | EIfThenElse (test, _, _) ->
        begin match test with
          | Pexp_apply ( fcall , _ ) ->
            let fcall = Utils.desc_of_expr fcall in
            begin match fcall with
              | Pexp_ident ({txt = Lident "not"}) ->
                mk_hint location (BPat IfCondNeg) src fix
              | _ -> None
            end
          | _ -> None
        end
      |_ -> None
    end
end


(* ------------------ Checks rule: if x then true else y  ------------------------------ *)
module IfReturnsTrue : CHECK = struct

  let fix = "rewriting using a boolean operator like && or ||"

  let check ({location;code; src} : lctxt) : hint option =
    let open Utils in
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "true"}, _),
            Pexp_ident ({txt = Lident y}) ->
            mk_hint location (BPat IfReturnsTrue) src fix
          | _ -> None
        end
      | _ -> None
    end
end


(* ------------------ Checks rule: if x then y else false ------------------------------ *)
module IfFailFalse : CHECK = struct

  let fix = "rewriting using a boolean operator like && or ||"

  let check ({location;code;src} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_else, b_then with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_ident ({txt = Lident y}) ->
            mk_hint location (BPat IfFailFalse) src fix
          | _ -> None
        end
      | _ -> None
    end
end


(* ------------------ Checks rule: if x then false else y  ----------------------------- *)
module IfSuccFalse : CHECK = struct

  let fix = "rewriting using a boolean operator like && or ||"

  let check ({location;code; src} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_construct ({txt = Lident "false"}, _),
            Pexp_ident ({txt = Lident y}) ->
            mk_hint location (BPat IfSuccFalse) src fix
          | _ -> None
        end
      | _ -> None
    end
end

(* ------------------ Checks rule: if x then y else true   ----------------------------- *)
module IfFailTrue : CHECK = struct

  let fix = "rewriting using && or ||"

  let check ({location;code;src} : lctxt) : hint option =
    begin match code with
      | EIfThenElse (_, b_then, b_else) ->
        begin match b_then, b_else with
          | Pexp_ident ({txt = Lident y}),
            Pexp_construct ({txt = Lident "true"}, _) ->
            mk_hint location (BPat IfFailTrue) src fix
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


