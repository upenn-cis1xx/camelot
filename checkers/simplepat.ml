open Asttypes
open Parsetree
open Longident
open Report
open Check    

module OneArmedMatch : CHECK = struct

  let fix = "Instead of writing a one armed pattern match, consider using a let statement"
    
  let check ({location;code;src} : lctxt) : hint option =
    begin match code with
      | PPatternMatch (e, [_]) ->
        mk_hint location (MPat OneArmedMatch) src fix
      | _ -> None
    end
end


let checks = [ OneArmedMatch.check
             ]
