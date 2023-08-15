
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


(* No Flagging here - only 2 match levels deep *)
let f () =
  let l = [] in
  begin match l with
    | [] ->
      begin match l with
        | [] -> true
        | _ -> false
      end
    | _ -> true
  end

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

(* shouldn't trigger *)
let z = (x = TConstr 3 || x = TConstr 4)
let z = (x = [12] || x = [50])
let z = (x = 5 || x = 6)
let z = (x = TConstr 3 && x = TConstr 4)
let z = (x = [12] && x = [50])
let z = (x = 5 && x = 6)

(* TODO: perhaps this one should trigger and say just change to true *)
let z = (x = TConstr 3 || not (x = TConstr 3))

(* should trigger *)
let z = (x = [] || x = [])
let z = (x = None || x = None)
let z = (x = 5 || x = 5)
let z = (x = TConstr 3 || x = TConstr 3)
let z = (x = TConstr 3 || x = TConstr 3 || x = TConstr 4)
let z = (x = TConstr 3 || x = TConstr 4 || x = TConstr 3)
let z = (x = [] && x = [])
let z = (x = None && x = None)
let z = (x = 5 && x = 5)
let z = (x = TConstr 3 && x = TConstr 3)
let z = (x = TConstr 3 && x = TConstr 3 && x = TConstr 4)
let z = (x = TConstr 3 && x = TConstr 4 && x = TConstr 3)
