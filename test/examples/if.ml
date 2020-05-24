let x = true
let y = false

let e = x

let beta = e


(* if e then true else false --> e *)
let t1 = if e then true else false

(* if e then false else true --> not e *)
let t2 = if e then false else true

(* if beta then beta else false -> beta *)
let t3 = if beta then beta else false

(* if not e then x else y -> if not e then y else x *)
let t4 = if not e then x else y

(* if x then true else y --> x || y *)
let t5 = if x then true else y

(* if x then y else false --> x && y *)
let t6 = if x then y else false

(* if x then false else y --> not x && y *)
let t7 = if x then false else y

(* if x then y else true *)
let t8 = if x then y else true
    
