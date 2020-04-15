open Asttypes
open Parsetree
open Longident
open Report
open Check    

module OneArmedMatch : CHECK = struct

  let fix = "using a let statement instead of a one-armed pattern match"

  let check ({location;code;src} : lctxt) : hint option =
    begin match code with
      | PPatternMatch (e, [_]) ->
        mk_hint location (MPat OneArmedMatch) src fix
      | _ -> None
    end
end


let checks = [ OneArmedMatch.check
             ]
