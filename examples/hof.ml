let rec plus_one l =
  match l with
  | [] -> []
   | h :: t -> h + 1 :: plus_one t

let rec plus_n n l =
  match l with
  | [] -> []
  | h :: t -> h + n :: plus_n n t

let rec print_l (l: int list) =
  match l with
  | [] -> ()
  | h :: t -> h |> string_of_int |> print_endline;
    print_l t

let rec sum_verbose (l: int list) =
  match l with
  | [] -> 0
  | h :: t -> h + sum_verbose t

