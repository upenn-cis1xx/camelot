
let display_verbose = Report.Display.student_display

(* Unfortunately, I have to copy most of the library code from camelot.ml *)

let fail msg = prerr_endline msg; exit 1

let safe_open src =
  try src, open_in src
  with Sys_error msg -> fail msg

let to_ast file =
  let src, f = safe_open file in
  src, ( f |> Lexing.from_channel |> Parse.implementation )

let line_lint : bool ref = ref false 

let lint_and_hint : (string * Parsetree.structure) -> unit = fun (file, ast) ->
  let store : Canonical.Hint.hint list ref = ref [] in
  let line_length_lint : string -> unit = fun file ->
    if not !line_lint then ()
    else
      let chan = open_in file in
      let lref : int ref = ref 1 in
      try
        while true; do
          let line = input_line chan in
          (if (String.length line > 80) then store := Canonical.Hint.line_hint file !lref line :: !store;);
          incr lref
        done; ()
      with End_of_file ->
        close_in chan; () in
  line_length_lint file;
  file |>
  Traverse.Iter.make_linterator store |>
  Traverse.Iter.apply_iterator ast;
  display_verbose !store


(* Run the tests in lexical.ml *)
let%expect_test _ =
  line_lint := true;
  let file : string = "./examples/lexical.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  line_lint := false;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 5, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 2, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 5, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/lexical.ml, line 2, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let verylongvariablenamethisispainful = [1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1]
    Consider:
    	indenting to avoid exceeding the 80 character line limit |}]

(* Run the tests in equality.ml *)
let%expect_test _ =
  let file : string = "./examples/equality.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 23, columns: 9-23
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 false = bfalse
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 22, columns: 9-21
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 true = btrue
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 21, columns: 9-23
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 bfalse = false
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 20, columns: 9-21
    Warning:
    	using `=` with a boolean literal
    You wrote:
    	 btrue = true
    Consider:
    	using the variable itself to represent the value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 15, columns: 8-41
    Warning:
    	using `=` with lists as a condition in an if statement
    You wrote:
    	 if [1; 2; 3] = q then None else x
    Consider:
    	using a pattern match to check whether a list has a certain value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 14, columns: 8-37
    Warning:
    	using `=` with lists as a condition in an if statement
    You wrote:
    	 if q = [1] then x else None
    Consider:
    	using a pattern match to check whether a list has a certain value

    (* ------------------------------------------------------------------------ *)
    File ./examples/equality.ml, line 5, columns: 8-23
    Warning:
    	using `=` with options
    You wrote:
    	 (Some 1) = (Some 1)
    Consider:
    	using a pattern match to check the presence of an option
  |}]

(* Run the tests in verbose.ml *)
let%expect_test _ =
  let file : string = "./examples/verbose.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 50, columns: 8-85
    Warning:
    	using nested if statements more than three layers deep
    You wrote:
    	 if x then 1 else if y then 2 else if x & y then 3 else if z = 4 then 3 else 9
    Consider:
    	using let statements or helper methods / rethinking logic

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, lines 32-43, columns: 2-5
    Warning:
    	using nested match statements more than three layers deep
    You wrote:
    	 match l with
    | [] ->
        (match l with
         | [] -> let z = [] in (match z with | _ -> true)
         | _ -> false)
    | _ -> true
    Consider:
    	using let statements or helper methods / rethinking logic

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 11, columns: 31-36
    Warning:
    	using fst / snd to project values out of a tuple
    You wrote:
    	 snd t
    Consider:
    	using a let pattern match statement instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 11, columns: 23-28
    Warning:
    	using fst / snd to project values out of a tuple
    You wrote:
    	 fst t
    Consider:
    	using a let pattern match statement instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 6, columns: 10-21
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 1 :: [] @ t
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 3, columns: 10-17
    Warning:
    	using `@` to prepend an element to a list
    You wrote:
    	 [1] @ t
    Consider:
    	using `::` instead

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 50, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 let z = if x then 1 else if y then 2 else if x & y then 3 else if z = 4 then 3 else 9
    Consider:
    	indenting to avoid exceeding the 80 character line limit

    (* ------------------------------------------------------------------------ *)
    File ./examples/verbose.ml, line 13, columns: 0-80
    Warning:
    	exceeding the 80 character line limit
    You wrote:
    	 (* Nested ifs :( - we skip local lets and sequencing to get the actual return type for now *)
    Consider:
    	indenting to avoid exceeding the 80 character line limit
  |}]

(* Run the tests in if.ml *)
let%expect_test _ =
  let file : string = "./examples/if.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 60, columns: 14-48
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if p h then nonsense p t else true
    Consider:
    	rewriting using a boolean operator like `||` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 55, columns: 14-45
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if p h then false else none p t
    Consider:
    	rewriting using a boolean operator like `&&` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 50, columns: 14-47
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if p h then forall p t else false
    Consider:
    	rewriting using a boolean operator like `&&`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 45, columns: 14-48
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if h = i then true else exists t i
    Consider:
    	rewriting using a boolean operator like `||`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 38, columns: 9-39
    Warning:
    	returning the condition of an if statement on success and a boolean literal otherwise
    You wrote:
    	 if 3 > 0 then 3 > 0 else false
    Consider:
    	returning just the condition or simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 34, columns: 20-41
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then x else true
    Consider:
    	rewriting using a boolean operator like `||` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 34, columns: 20-41
    Warning:
    	returning the condition of an if statement on success and a boolean literal otherwise
    You wrote:
    	 if x then x else true
    Consider:
    	returning just the condition or simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 31, columns: 9-30
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then y else true
    Consider:
    	rewriting using a boolean operator like `||` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 28, columns: 9-31
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then false else y
    Consider:
    	rewriting using a boolean operator like `&&` and `not`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 25, columns: 9-31
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then y else false
    Consider:
    	rewriting using a boolean operator like `&&`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 22, columns: 9-30
    Warning:
    	overly verbose if statement that can be simplified
    You wrote:
    	 if x then true else y
    Consider:
    	rewriting using a boolean operator like `||`

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 19, columns: 9-31
    Warning:
    	checking negation in the if condition
    You wrote:
    	 if not e then x else y
    Consider:
    	swapping the then and else branches of the if statement

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 16, columns: 9-37
    Warning:
    	returning the condition of an if statement on success and a boolean literal otherwise
    You wrote:
    	 if beta then beta else false
    Consider:
    	returning just the condition or simplifying further

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 13, columns: 9-34
    Warning:
    	using an if statement to return `true | false` literally
    You wrote:
    	 if e then false else true
    Consider:
    	returning just the condition (+ some tweaks)

    (* ------------------------------------------------------------------------ *)
    File ./examples/if.ml, line 10, columns: 9-34
    Warning:
    	using an if statement to return `true | false` literally
    You wrote:
    	 if e then true else false
    Consider:
    	returning just the condition (+ some tweaks)
  |}]

(* Run the tests in match.ml *)
let%expect_test _ =
  let file : string = "./examples/match.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 112, columns: 13-20
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | y :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [y] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 112, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 106, columns: 7-14
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 100, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 81-82, columns: 2-15
    Warning:
    	using pattern matching on a tuple (for fewer than 2 cases)
    You wrote:
    	 match r with | (x, y) -> ()
    Consider:
    	using a let statement to extract tuple fields

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 69-71, columns: 2-16
    Warning:
    	using pattern matching on a record (for fewer than 3 cases)
    You wrote:
    	 match r with | { x; y } -> () | { x;_} -> ()
    Consider:
    	using a let statement to extract record fields

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 65-66, columns: 2-16
    Warning:
    	using pattern matching on a record (for fewer than 3 cases)
    You wrote:
    	 match r with | { x; y } -> ()
    Consider:
    	using a let statement to extract record fields

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 48, columns: 4-13
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | abc :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [abc] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 47, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | x :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [x] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, line 42, columns: 4-11
    Warning:
    	using an overly complex match clause
    You wrote:
    	 | _ :: [] -> ...
    Consider:
    	expressing this match case more compactly, such as: | [_] -> ...

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 12-15, columns: 15-5
    Warning:
    	using integer pattern matching on fewer than 3 cases
    You wrote:
    	 match b with | 2 -> true | 3 -> false
    Consider:
    	using an if statement and `=`

    (* ------------------------------------------------------------------------ *)
    File ./examples/match.ml, lines 4-7, columns: 15-5
    Warning:
    	using pattern matching on boolean literals
    You wrote:
    	 match b with | false -> true | true -> false
    Consider:
    	using an if statement or boolean operators
  |}]

(* Run the tests in hof.ml *)
let%expect_test _ =
  let file : string = "./examples/hof.ml" in
  let to_lint = to_ast file in
  lint_and_hint to_lint;
  [%expect{|
    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 17-20, columns: 0-31
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec sum_verbose (l : int list) =
      match l with | [] -> 0 | h::t -> h + (sum_verbose t)
    Consider:
    	using a higher order function like fold

    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 11-15, columns: 0-13
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec print_l (l : int list) =
      match l with
      | [] -> ()
      | h::t -> ((h |> string_of_int) |> print_endline; print_l t)
    Consider:
    	using a higher order function like iter

    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 6-9, columns: 0-33
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec plus_n n l =
      match l with | [] -> [] | h::t -> (h + n) :: (plus_n n t)
    Consider:
    	using a higher order function like transform

    (* ------------------------------------------------------------------------ *)
    File ./examples/hof.ml, lines 1-4, columns: 0-34
    Warning:
    	overly verbose function implementation
    You wrote:
    	 let rec plus_one l =
      match l with | [] -> [] | h::t -> (h + 1) :: (plus_one t)
    Consider:
    	using a higher order function like transform
  |}]
