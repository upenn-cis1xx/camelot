(* List of rules to lint *)
val rules : (Style.hint list ref -> Style.patternctxt -> unit) list 

(* List of patterns found *)
val pats : Style.patternctxt list ref

(* Parses patterns of interest - can store these anywhere you like or not at all *)
val patterns_of_interest : string -> Parsetree.expression -> unit

(* Checks rules against the patterns found *)
val lint : unit -> unit

(* Prints linter contents *)
val hint : unit -> unit
