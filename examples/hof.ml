let rec plus_one l =
  match l with
  | [] -> []
  | h :: t -> h + 1 :: plus_one t
