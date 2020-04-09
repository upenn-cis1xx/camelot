open Check
open Report
open Astutils

module EqList : CHECK = struct
      
  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | EqApply (lhs, rhs) ->
        (if is_list_literal lhs || is_list_literal rhs then
          Some ({
              loc = location;
              violation = (EqPat EqList)
            })
        else
          None)
      | _ -> None
    end
end




module EqOption : CHECK = struct
  let check ({location;code} : lctxt) : warn option =
    match code with
    | EqApply (lhs, rhs) ->
      (
        if is_option_lit lhs || is_option_lit rhs then
          Some ({ loc = location; violation = (EqPat EqOption) })
        else None
          
      )
    | _ -> None

end



let checks = [ EqList.check
             ; EqOption.check
             ]  
                    
