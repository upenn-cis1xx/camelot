type tf =
  | PatA

let x : tf -> int = fun x -> match x with | PatA -> 1

let x = if true then true else false
