open Check
open Style
open Astutils
(* Reccomends the use of map over a more literal variant *)

module HofMap : CHECK = struct
  open Check
  let fix = "rewriting using List.map : ('a -> 'b) -> 'a list -> 'b list"

  let check st {location; source; pattern} = 
    begin match pattern with
      | EqApply (lhs, rhs) ->
        if is_list_literal lhs || is_list_literal rhs then
          st := mk_hint location source fix (EqPat EqList) :: !st;
      | _ -> ()
    end
end