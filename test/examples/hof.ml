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

(* these functions should not be flagged because they shortcircuit *)
let rec int_member (i : int) (l : int list) : bool =
  match l with 
  | [] -> false 
  | x::xs -> (i = x) || (int_member i xs)

let rec string_member (s: string) (l: string list) : bool =
  begin match l with
  | [] -> false
  | x :: xs -> s = x || string_member s xs
  end

(* this function _should_ be flagged since it reimplements List.fold_right *)
let rec fold (combine: 'a -> 'b -> 'b) (base: 'b) (l: 'a list) : 'b =
  begin match l with
  | [] -> base
  | x :: xs -> combine x (fold combine base xs)
  end