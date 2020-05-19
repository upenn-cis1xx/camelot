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
         
