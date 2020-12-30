let example1 l =
  (* Test for eq list being disables *)
  l = [1;2;3]

let example2 o =
  (* Test for option equality *)
  if o = None then [] else [1;2;3]

let test x =
  (* Test for bool equality - should trigger in this file *)
  x = true
