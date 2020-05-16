open Warnloc


type violation = string
type fix = string

type hint = {
loc: warn_loc
; raw: string
; fix: fix
; violation : string
}

let mk_hint loc raw fix violation = {loc; raw; fix; violation}






