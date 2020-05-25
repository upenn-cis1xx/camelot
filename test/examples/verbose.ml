
(* Prepending with list construction and append, where cons should be used *)
let f t = [1] @ t

(* Likewise, but even more verbose *)
let f t = 1 :: [] @ t


(* Tuple projection bad! *)

let f (t: int * int) = fst t + snd t

(* Nested ifs :( - we skip local lets and sequencing to get the actual return type for now *)
let x = true
let y = false

let dounit () = ()
(* No Flagging here - only two levels deep *)
let f () = if x then (if x then x else y) else y
let f () = if x then
    (if x then x
     else let z = 3 in
       dounit();
       (if z = 2 then x else y) )
  else y


(* Nested matched bad as well *)

let f () =
  let l = [] in
  begin match l with
    | [] ->
      begin match l with
        | [] ->
          let z = [] in
        begin match z with
          | _ -> true
        end
      | _ -> false
    end
  | _ -> true
  end


(* If statement three layers deep *)
let z = if x then 1 else if y then 2 else if x & y then 3 else 4

(* If statement four layers deep *)
let z = if x then 1 else if y then 2 else if x & y then 3 else if z = 4 then 3 else 9