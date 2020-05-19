open Canonical
open Utils
open Astutils

module LitPrepend : Check.CHECK = struct
  let fix = "using `::` instead"
  let violation = "using `@` to prepend an element to a list"
  let check st ({location; source = _; pattern} : Pctxt.patternctxt) =
    begin match pattern with
      | Pexp_apply (application, [(_, lop); _]) ->
        if application =~ "@" && is_singleton_list lop then
          let raw = IOUtils.read_at_loc location in
          st := Hint.mk_hint location raw fix violation :: !st
      | _ -> ()
    end
end


