open Check
open Style
open Astutils

module EqList : CHECK = struct
  open Check
  let fix = "using a pattern match to check whether a list has a certain value"

  let check st {location; source; pattern} = 
    begin match pattern with
      | EqApply (lhs, rhs) ->
        if is_list_literal lhs || is_list_literal rhs then
          st := mk_hint location source fix (EqPat EqList) :: !st;
      | _ -> ()
    end
end




module EqOption : CHECK = struct

  let fix = "using a pattern match to unwrap the option"

  let check st {location; source; pattern} =
    match pattern with
    | EqApply (lhs, rhs) -> 
      if is_option_lit lhs || is_option_lit rhs 
      then st := mk_hint location source fix (EqPat EqOption) :: !st;
    | _ -> ()

end



let checks = [ EqList.check
             ; EqOption.check
             ]  

