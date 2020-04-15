type tf =
  | PatA

let x : tf -> int = fun x ->
  match x with
  | PatA -> 1

