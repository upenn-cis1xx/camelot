open Check
open Report


module EqList : CHECK = struct

  let is_list (e : exp) : bool =
    match e with
    (* Nil case *)
    | Pexp_construct ({txt = Lident "[]"} , _) -> true
    (* Cons case *)
    | Pexp_construct ({txt = Lident "::"}, _) -> true
    | _ -> false
      
  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | EqApply (lhs, rhs) ->
        (if is_list lhs || is_list rhs then
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

  let is_option (e : exp) : bool =
    match e with
    (* None case *)
    | Pexp_construct ({txt = Lident "None"}, _) -> true
  (* Some case *)
    | Pexp_construct ({txt = Lident "Some"}, _) -> true
    | _ -> false
      
  
  let check ({location;code} : lctxt) : warn option =
    match code with
    | EqApply (lhs, rhs) ->
      (
        if is_option lhs || is_option rhs then
          Some ({ loc = location; violation = (EqPat EqOption) })
        else None
          
      )
    | _ -> None

end



let checks = [ EqList.check
             ; EqOption.check
             ]  
                    
(*


    begin match code with
      | EqApply (lhs, rhs) ->
        (if is_option lhs || is_option rhs then
          Some ({
              loc = location;
              violation = (EqPat EqOption)
            })
        else None)
      | _ -> None
    end


*)
