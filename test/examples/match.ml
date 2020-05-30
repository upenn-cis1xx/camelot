(*Pattern match with booleans*)
let b () = 
  let b = false in 
  let result = begin match b with 
    | false -> true
    | true -> false
  end in ()
               
(*Pattern match with numbers*)
let b () = 
  let b = 1 in 
  let result = begin match b with 
    | 2 -> true
    | 3 -> false
  end in ()

(*Pattern match with a long list of strings (this is considered OK)*)
let b () = 
  let b = "Tuesday" in 
  let result = begin match b with 
    | "Monday" -> "Tuesday"
    | "Tuesday" -> "Friday"
    | "Wednesday" -> "Thursday"
    | "Thursday" -> "Friday"
    | "Friday" -> "Saturday"
    | "Saturday" -> "Sunday"
    | "Sunday" -> "Monday"
    | _ -> failwith "not a valid day"
  end in ()

(*Pattern match with lists*)
let b () = 
  let b = [] in 
  let result = begin match b with 
    | [] -> true
    | [3] -> false
  end in ()

(* Pattern matching with a overly complex cons case *)
let b l =
  match l with
  | _ :: [] -> ()
  | _ -> ()

let b l =
  match l with
  | x :: [] -> ()
  | x :: y :: [] -> ()
  | _ -> ()

let b l1 l2 =
  match l1, l2 with
  | x :: [], _ -> ()
  | x :: y :: [], _ -> ()
  | _ -> ()

(* should not fire *)
let b l =
  match l with
  | [x] -> ()
  | [x; y] -> ()
  | _ -> ()


(* Pattern matching on a record *)
type t = {x: int; y: int}
         
let b (r: t) =
  match r with
  | {x; y} -> ()

let b (r: int * int) =
  match r with
  | (x,y) -> ()

let b (r: int * int) =
  match r with
  | (1,2) -> ()
  | (3,4) -> ()

(* Should not fire (3 or more) *)
let b (r: int * int) =
  match r with
  | (1,2) -> ()
  | (3,4) -> ()
  | (5,6) -> ()
             
