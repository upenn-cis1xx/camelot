
(* Optional Equality *)
let x = None
let a = x = None
let b = Some 1 = Some 1

(* List equality *)

(* shouldn't raise issues *)
let c = [] = [1]
let d = [1;2;3] = []
let q = [1;2;3]
(* Should raise issues *)
let e = if (q = [1]) then x else None
let f = if ([1;2;3] = q) then None else x
