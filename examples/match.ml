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


type t = {x: int; y: int}
(* Pattern matching on a record *)
         
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
             
