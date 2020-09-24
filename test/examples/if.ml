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

(* Should ding them twice - encouragement to fix *)
let double_points = if x then x else true

(* Slightly more complex examples *)

let t9 = if 3 > 0 then 3 > 0 else false

(* comparing boolean literals *)
(* x = true --> x, y = false ---> not y *)
let t10 = t1 = true 
let t11 = t2 = false
let t12 = true = t1
let t13 = false = t2

(* More complex examples *)

let rec exists (l : int list) (i : int) =
  match l with
  | [] -> false
  | h :: t -> if h = i then true else exists t i

let rec forall (p: 'a -> bool) (l : 'a list) =
  match l with
  | [] -> true
  | h :: t -> if p h then forall p t else false

let rec none (p: 'a -> bool) (l: 'a list) =
  match l with
  | [] -> true
  | h :: t -> if p h then false else none p t

let rec nonsense (p: 'a -> bool) (l : 'a list) =
  match l with
  | [] -> false
  | h :: t -> if p h then nonsense p t else true
