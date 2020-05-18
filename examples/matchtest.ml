(*Pattern match with booleans*)
let b = false in 
begin match b with 
  | false -> failwith ""
  | true -> failwith ""
end