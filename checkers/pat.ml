open Report
    
module OneArmedMatch : CHECK = struct

  let check ({location;code} : lctxt) : warn option =
    begin match code with
      | PPatternMatch (e, [_]) -> Some ({
          loc=location;
          violation = (MPat (OneArmedMatch))
        }) 
      | -> None
    end
end


let checks = [ OneArmedMatch.check
             ]
