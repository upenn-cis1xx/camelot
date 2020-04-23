
(* Should reccommend the use of map *)
let rec shouldMap (l: int list) : int list = 
  let b1 = 3 in
  let b2 = l in
  match l with
  | [] -> let y = [] in y
  | h :: t -> h + 1 :: shouldMap t