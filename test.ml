(* Lint example, indexed by violation *)

let t = true
let f = false
  

let v0 = if t then true else false
let v1 = if t then false else true
let v2 = if t then t else false
let v3 = if not t then true else false
let v4 = if t then true else f
let v5 = if t then f else false
let v6 = if t then false else f
let v7 = if f then false else true

