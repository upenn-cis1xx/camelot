
(*  Should be ok *)
let conv _ = ["allo"]


(* Should not raise a linter error *)
let test _ =
  ["1"] = conv ()


(* Should raise a linter error *)
let not_called_test _ =
  ["a"] = conv ()

(* Should also not raise a linter error *)
let test _ =
  ["2"] = conv ()

(* Shouldn't raise a linter error - hof was disabled *)
let rec thing l =
  match l with
  | [] -> []
  | h :: t -> h + 1 :: thing t
