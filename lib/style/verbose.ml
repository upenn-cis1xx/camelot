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


module TupleProj : Check.CHECK = struct
  let fix = "using a let pattern match statement instead"
  let violation = "using fst / snd to project values out of a tuple"
  let check st ({location; source; pattern} : Pctxt.patternctxt) =
    begin match pattern with
      | Pexp_apply (application, [_]) ->
        if application =~ "fst" || application =~ "snd" then
          st := Hint.mk_hint location source fix violation :: !st
      | _ -> ()
    end
end
