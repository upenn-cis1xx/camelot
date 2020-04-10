open Check
open Report
open Astutils

module EqList : CHECK = struct

  let fix = "using a pattern match to check whether a list has a certain value"
    
  let check ({location;code;src} : lctxt) : hint option =
    begin match code with
      | EqApply (lhs, rhs) ->
        if is_list_literal lhs || is_list_literal rhs then
          mk_hint location (EqPat EqList) src fix
        else None
      | _ -> None
    end
end




module EqOption : CHECK = struct

  let fix = "using a pattern match to unwrap the option"
    
  let check ({location;code;src} : lctxt) : hint option =
    match code with
    | EqApply (lhs, rhs) ->
      (
        if is_option_lit lhs || is_option_lit rhs then
          mk_hint location (EqPat EqOption) src fix
        else None
          
      )
    | _ -> None

end



let checks = [ EqList.check
             ; EqOption.check
             ]  
                    
