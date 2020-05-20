
(* Prepending with list construction and append, where cons should be used *)
let f t = [1] @ t

(* Likewise, but even more verbose *)
let f t = 1 :: [] @ t


(* Tuple projection bad! *)

let f (t: int * int) = fst t + snd t
